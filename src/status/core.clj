(ns status.core
  (:require [status routers compilers utils]
            [me.raynes.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [ring.adapter.jetty :as jetty]
            [ring.util.response :as jetty-response]
            [clostache.parser :as cp]))

(defn- compile-specs
  [specs in-path]
  (letfn [(helper [{:keys [paths router compiler]
                    :as spec}]
            (merge {:router status.routers/id
                    :compiler status.compilers/copy}
                   (assoc spec
                          :paths
                          (set (map #(.getAbsolutePath %)
                                    (fs/glob (fs/file in-path paths)))))))]
    (vec (map helper specs))))

(defn- put-content
  [template content]
  (string/replace template "$content$" content))

(defn- compile-template
  [template-name m cache]
  (let [{:keys [file-path compiler metadata-parsers context]
         :as spec}                                           (m template-name)]
    (with-open [in-stream (io/reader file-path)
                string-writer (java.io.StringWriter.)
                out-stream (io/writer string-writer)]
      (let [metadata (status.utils/read-metadata in-stream metadata-parsers)
            compiled (do (compiler in-stream
                                   out-stream
                                   (merge context metadata))
                         (.flush out-stream)
                         (.toString string-writer))
            [ncache put-in] (cond
                              (and (contains? metadata :template)
                                   (contains? cache
                                              (:template metadata)))
                              (vector cache
                                      (put-content (get-in cache [(:template
                                                                    metadata)
                                                                  :template])
                                                   compiled))

                              (contains? metadata :template)
                              (let [dependency (:template metadata)
                                    new-cache (compile-template dependency
                                                                m
                                                                cache)]
                                (vector new-cache
                                        (put-content (get-in cache [(:template
                                                                      metadata)
                                                                    :template])
                                                     compiled)))

                              :else (vector cache compiled))]
        (assoc ncache
               template-name
               {:template put-in
                :metadata metadata})))))

(defn- compile-templates
  [template-specs in-path]
  (let [s (mapcat (fn [{:keys [paths]
                        :as spec}]
                    (map #(vector (fs/base-name %)
                                  (assoc (dissoc spec
                                                 :paths
                                                 :templates
                                                 :router)
                                         :file-path %))
                         paths))
                  template-specs)]
    (if (not= (count (set (map first s))) (count s))
      (throw (IllegalArgumentException. "Duplicate names for templates."))
      (loop [remaining (apply hash-map (apply concat s))
             cache {}]
        (if (not (seq remaining))
          cache
          (let [k (key (first remaining))
                new-cache (compile-template k remaining cache)
                new-remaining (apply dissoc remaining (keys new-cache))]
            (recur new-remaining new-cache)))))))

(defn compile-paths
  [in-path out-path compiled-templates context]
  (fn [{:keys [paths router compiler templates metadata-parsers binary]
        :or {metadata-parsers {}}}]
    (when-not templates
      (dorun (map (fn [path]
                    (let [file-path (router
                                      (status.utils/output-of path
                                                              in-path
                                                              out-path)
                                      out-path)]
                      (status.utils/make-dirs-in-path file-path)
                      (if binary
                        (with-open [in-stream (io/input-stream path)
                                    out-stream (io/output-stream file-path)]
                          (compiler in-stream out-stream context))
                        (with-open [in-stream (io/reader path)]
                          (let [metadata (status.utils/read-metadata
                                           in-stream
                                           metadata-parsers)
                                compiled (with-open [string-writer
                                                     (java.io.StringWriter.)

                                                     out-stream
                                                     (io/writer string-writer)]
                                           (compiler in-stream
                                                     out-stream
                                                     (merge context
                                                            (get-in
                                                              compiled-templates
                                                              [(:template
                                                                 metadata)
                                                               :metadata])
                                                            metadata))
                                           (.close out-stream)
                                           (.toString string-writer))
                                put-in (cp/render
                                         (if (contains? metadata :template)
                                           (put-content
                                             (get-in compiled-templates
                                                     [(:template metadata)
                                                      :template])
                                             compiled)
                                           compiled)
                                         (merge
                                           context
                                           (if (contains? metadata :template)
                                             (get-in compiled-templates
                                                     [(:template metadata)
                                                      :metadata])
                                             {})
                                           metadata))]
                            (with-open [out-stream (io/writer file-path)]
                              (.write out-stream put-in)))))))
                  paths)))))

(defn- run-build
  [{:keys [in-path out-path specs compiled-templates]}
   context]
  (dorun (map (compile-paths in-path out-path compiled-templates context)
              specs)))

(defn- serve-static
  [out-path]
  (fn [req]
    (let [mime-types {}]
      (when-let [f (jetty-response/file-response (:uri req)
                                                 {:root out-path})]
        (if-let [mime-type (mime-types (re-find #"\..+$" (:uri req)))]
          (merge f {:headers {"Content-Type" mime-type}})
          f)))))

(defn make-engine
  [& {:keys [in-dir out-dir specs]}]
  (if (some nil? [in-dir out-dir specs])
    (throw (IllegalArgumentException.
             "You must pass in :in-dir, :out-dir and :specs"))
    (let [in-path (.getAbsolutePath (fs/file in-dir))]
      {:in-path in-path
       :out-path (.getAbsolutePath (fs/file out-dir))
       :specs (compile-specs specs in-path)})))

(defn execute
  [{:keys [in-path out-path specs]
    :as engine}
   [command & args]
   context]
  (case command
    "build" (let [templates (compile-templates (filter :templates specs)
                                               in-path)]
              (run-build (assoc engine :compiled-templates templates) context))
    "clean" (fs/delete-dir out-path)
    "jetty" (jetty/run-jetty (serve-static out-path)
                             {:port 8080})
    nil (println "Please specify a command.")
    (println "Unknown command.")))
