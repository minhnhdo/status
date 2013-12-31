(ns status.routers
  (:require [me.raynes.fs :as fs]))

(defn id [path _] path)

(defn set-extension
  [ext]
  (fn [path _]
    (let [n (fs/base-name path true)]
      (.getAbsolutePath (apply fs/file (concat (butlast (fs/split path))
                                               [(str n "." ext)]))))))
