(ns aidbox-cli
  (:gen-class)
  (:require [zen.cli]
            [zen.core]
            [generator.cli]
            [ftr.zen-cli]))

(defn -main [& args]
  (let [ztx (zen.core/new-context)
        _ (zen.core/read-ns ztx 'aidbox-cli)]
      (zen.cli/cli ztx 'aidbox-cli/config args)
      (System/exit 0)))
