(ns zen-migration.meta-resource-to-zen
  (:require [clojure.java.io :as io]
            [clojure.set]
            [clojure.string]
            [zen.core]
            [clojure.edn :as edn]
            [clj-http.client :as http]))

(def fhir-primitive->zen-primitive
  '{"boolean" {:type zen/boolean}
    "CodeableConcept" {:confirms #{hl7-fhir-r4-core.CodeableConcept/schema}}
    "decimal"     {:type zen/number}
    "integer"     {:type zen/integer}
    "number"     {:type zen/number}
    "unsignedInt" {:type zen/integer
                   :min  0}
    "positiveInt" {:type zen/integer
                   :min  1}
    "string"       {:type      zen/string}
    "markdown"     {:type      zen/string}
    "id"           {:type  zen/string
                    :regex "^[A-Za-z0-9\\-\\.]{1,64}$"}
    "uuid"         {:type  zen/string}
    "oid"          {:type  zen/string
                    :regex "^urn:oid:[0-2](\\.(0|[1-9][0-9]*))+$"}
    "uri"          {:type zen/string}
    "url"          {:type zen/string}
    "canonical"    {:type zen/string}
    "code"         {:type  zen/string
                    :regex "^[^\\s]+(\\s[^\\s]+)*$"}
    "base64Binary" {:type  zen/string
                    :regex "^(\\s*([0-9a-zA-Z\\+\\=\\/]){4})++\\s*$"}
    "xhtml"        {:type zen/string}

    "instant"  {:type  zen/string
                :regex "^([0-9]([0-9]([0-9][1-9]|[1-9]0)|[1-9]00)|[1-9]000)-(0[1-9]|1[0-2])-(0[1-9]|[1-2][0-9]|3[0-1])T([01][0-9]|2[0-3]):[0-5][0-9]:([0-5][0-9]|60)(\\.[0-9]+)?(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))$"}
    "dateTime" {:type  zen/string
                :regex "^([0-9]([0-9]([0-9][1-9]|[1-9]0)|[1-9]00)|[1-9]000)(-(0[1-9]|1[0-2])(-(0[1-9]|[1-2][0-9]|3[0-1])(T([01][0-9]|2[0-3]):[0-5][0-9]:([0-5][0-9]|60)(\\.[0-9]+)?(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00)))?)?)?$"}
    "date"     {:type  zen/string
                :regex "^([0-9]([0-9]([0-9][1-9]|[1-9]0)|[1-9]00)|[1-9]000)(-(0[1-9]|1[0-2])(-(0[1-9]|[1-2][0-9]|3[0-1]))?)?$"}
    "time"     {:type  zen/string
                :regex "^([01][0-9]|2[0-3]):[0-5][0-9]:([0-5][0-9]|60)(\\.[0-9]+)?$"}})

(def context (atom {}))

(declare get-type)

(defn generate-namespace [name]
  (symbol (str "custom." name)))

(defn get-wrapper [name schema]
  (let [ns-name (generate-namespace name)
        import (set (map #(symbol (clojure.string/replace %1 #"/schema" "")) (:reference ((keyword name) @context))))]
    {ns-name {'ns ns-name,
              'import (clojure.set/union import #{'zen.fhir 'zenbox 'aidbox.repository.v1})
              'schema schema}}))

(def avoid-keys #{:isRequired :search :isCollection :extensionUrl :refers})

(defn read-versions [ztx path]
  (println "Reading zen packages...")
  (with-open [zen-project (io/reader (str path "/zen-package.edn"))]
    (mapv (fn [package]
            (println "Reading " (first package))
            (zen.core/read-ns ztx (symbol (first package)))) (:deps (edn/read (java.io.PushbackReader. zen-project))))))

(defn getReferences
  [ztx references]
  (let [version (namespace (first (zen.core/get-tag ztx 'zen.fhir/base-schemas)))
        schema (:schemas (zen.core/get-symbol ztx (symbol (str version "/base-schemas"))))]
    (set (mapv (fn [item]
                 (let [schema-key (first (vals (second (first (filter (fn [i] (= (first i) item)) schema)))))
                       reference (if schema-key schema-key (symbol (str (generate-namespace item) "/schema")))]
                   (swap! context update-in
                          [(:current @context) :reference]
                          #(conj %
                                 (symbol reference)))
                   reference))
               references))))

(defn get-require [data]
  (filter (fn [key]
            (:isRequired (key (:attrs data)))) (keys (:attrs data))))

(defn set-data-recursively [parse-data ztx data]
  (let [require (get-require data)]
    (reduce (fn [acc key]
              (-> acc (merge (if (> (count require) 0) {:require (set require)} {}))
                  (merge (cond (= key :attrs) {:keys (parse-data ztx (key data) key) :type 'zen/map}
                               (= key :type) (if (= (key data) "Reference")
                                               {:confirms #{'zen.fhir/Reference}
                                                :zen.fhir/reference {:refers (getReferences ztx (:refers data))}}
                                               (get-type ztx (key data)))

                               (= key :description) {:zen/desc (:description data)}
                               (= key :isOpen) {:validation-type :open}
                               :else {key (parse-data ztx (key data) key)}))))
            {} (filter #(not (contains? avoid-keys %)) (keys data)))))

(defn parse-data [ztx data key]
  (cond
    (:isCollection data)
    {:type 'zen/vector :every (set-data-recursively parse-data ztx data)}
    (= key :enum)
    (mapv (fn [item] {:value item}) data)
    (map? data)
    (set-data-recursively parse-data ztx data)

    :else nil))

(defn get-type [ztx data]
  (cond
    (fhir-primitive->zen-primitive data)
    (fhir-primitive->zen-primitive data)
    (map? data)
    {:type (set-data-recursively parse-data ztx data)}
    :else {:validation-type :open}))

(defn get-adibox-creds [path]
  (reduce (fn [acc item]
            (let [[key value] (clojure.string/split item #"=")]
              (assoc acc (keyword key) value)))
          {} (line-seq (io/reader path))))

(defn get-aidbox-apps [{aidbox-url :AIDBOX_URL
                        aidbox-client :AIDBOX_CLIENT
                        aidbox-secret :AIDBOX_SECRET}]
  (http/get (str aidbox-url "App") {:basic-auth [aidbox-client aidbox-secret] :as :json}))

(defn migrate-to-zen [path-to-zen path-to-creds]
  (let [ztx  (zen.core/new-context {:package-paths [path-to-zen]})
        _ (read-versions ztx path-to-zen)
        creads (get-adibox-creds path-to-creds)
        apps (:entry (:body (get-aidbox-apps creads)))
        default-values {:zen/tags   #{'zen/schema 'zen.fhir/base-schema 'aidbox.repository.v1/repository}
                        :confirms   #{'zen.fhir/Resource}
                        :extra-parameter-sources :all
                        :zen.fhir/version "0.5.11"}]

    (io/make-parents "")

    (println "Generation folder structure")
    (io/make-parents "zen-packages/custom/zrc/custom/empty")
    (spit "zen-packages/custom/zrc/custom.edn" "")

    (spit "zen-packages/custom/zrc/custom.edn",  "{ns custom\n import #{zen.fhir}\n base-schemas {:zen/tags #{zen.fhir/base-schemas}\n :schemas {")

    (->> apps
         (mapv :resource)
         (mapv (fn [{:keys [entities]}]
                 (->> (reduce (fn [acc key]
                                (spit "zen-packages/custom/zrc/custom.edn" (str (str "\"" (name key) "\"") " { \"" (name key) "\" #zen/quote " (generate-namespace (name key)) "/schema }\n") :append true)
                                (swap! context assoc key {:reference [], :namespaces [], :require {}})
                                (swap! context assoc :current key)
                                (assoc acc key (merge default-values {:zen.fhir/type (name key)} (parse-data ztx (key entities) key))))
                              {} (keys entities))
                      (mapv (fn [[key value]]
                              (let [wrapper (get-wrapper (name key) value)]
                                (spit (str "zen-packages/custom/zrc/custom/" (name key) ".edn") (second (first wrapper)))
                                wrapper)))))))
    (spit "zen-packages/custom/zrc/custom.edn",  "}}\nig {:zen/tags #{zen.fhir/ig},:base-schemas base-schemas}}" :append true)
    :ok))

(comment
  (migrate-to-zen "/Users/ross/Desktop/HS/cli/creds.txt" "/Users/ross/Desktop/HS/cli/zen-project"))

