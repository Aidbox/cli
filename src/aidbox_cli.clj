(ns aidbox-cli
  (:gen-class)
  (:require [zen.cli]
            [zen.core]
            [generator.cli]
            [ftr.zen-cli]
            [zen-migration.meta-resource-to-zen]
            [clojure.string]
            [clojure.edn]))

(defn migrate-to-zen [args]
  (zen-migration.meta-resource-to-zen/migrate-to-zen (zen.cli/get-pwd) (last (clojure.string/split (str (first args)) #"="))))

(defmethod zen.cli/command 'aidbox-cli/migrate-to-zen [_ args _]
  (migrate-to-zen args))

(defn -main [& args]
  (let [ztx (zen.core/new-context)
        _ (zen.core/read-ns ztx 'aidbox-cli)]
    (zen.cli/cli ztx 'aidbox-cli/config args)
    (System/exit 0)))
