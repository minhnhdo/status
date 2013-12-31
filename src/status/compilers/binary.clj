(ns status.compilers.binary)

(defn copy
  [^java.io.BufferedInputStream in-stream
   ^java.io.BufferedOutputStream out-stream
   _]
  (let [buffer (byte-array 8192)]
    (loop []
      (let [len (.read in-stream buffer)]
        (when (>= len 0)
          (.write out-stream buffer 0 len)
          (recur))))))
