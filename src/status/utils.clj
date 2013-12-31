(ns status.utils
  (:require [clojure.string :as string]
            [me.raynes.fs :as fs]))

(defn debug [x] (prn x) x)

(defn same-front
  [s1 s2]
  (take-while #(= (first %) (second %))
              (map #(vector % %2) s1 s2)))

(defn merge-paths
  [from to]
  (let [from-comps (fs/split from)
        to-comps (fs/split to)
        same-front-count (count (same-front from-comps to-comps))]
    (concat (repeat (count (drop same-front-count from-comps)) "..")
            (drop same-front-count to-comps))))

(defn make-dirs-in-path
  [path]
  (fs/mkdirs (apply fs/file (butlast (fs/split path)))))

(defn output-of
  [path in-path out-path]
  (string/replace-first path in-path out-path))

(defn urls-of
  [path {:keys [in-path out-path specs]}]
  (for [{:keys [paths router]} specs]
    (when (contains? paths path)
      (let [merged (merge-paths out-path
                                (router (output-of path in-path out-path)
                                        out-path))
            separators (repeat "/")]
        (apply str ((if (= ".." (first merged))
                      interpose
                      interleave) separators merged))))))

(defn read-metadata
  [^java.io.BufferedReader in-stream fn-map]
  ; attempt to read the first 4 characters
  (.mark in-stream 4)
  (let [delimiter "---"
        buffer (char-array 4)]
    (.read in-stream buffer)
    (if (not= (String. buffer) (str delimiter "\n"))
      ; no metadata found, reset the in-stream
      (do (.reset in-stream)
          {})
      ; read metadata
      (loop [m {}]
        (if-let [line (.readLine in-stream)]
          (if (not= line delimiter)
            (let [[k v] (string/split line #": " 2)
                  k' (keyword k)
                  v' (or ((get fn-map k' identity) v) true)]
              (recur (assoc m k' v')))
            m)
          m)))))
