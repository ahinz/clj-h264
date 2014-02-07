(ns h264.core
  (:require [h264.bits :as bits]
            [h264.sei :as sei]
            [h264.params :as params])
  (:import [java.nio ByteBuffer]
           [java.nio.channels FileChannel$MapMode]
           [java.io FileInputStream File]))

(set! *warn-on-reflection* true)

(defn- get-mapped-buffer [^String path]
  (let [file (File. path)]
    (.map (.getChannel (FileInputStream. file))
          FileChannel$MapMode/READ_ONLY
          0
          (.length file))))

(defn afile [] (get-mapped-buffer "out.h264"))

(defn move-to-start-of-nal [buffer]
  (if (or (= (bits/next-bytes buffer 3) 1)
          (= (bits/next-bytes buffer 4) 1))
    buffer
    (skip buffer 1)))

(defn discard-bytes [buffer n tgt]
  (let [actual (bits/next-bytes buffer n)]
    (if (= actual tgt)
      (skip buffer n)
      (throw (Exception. (str "expected " tgt " got " actual))))))

(defn get-size-of-nal
  ([buffer] (get-size-of-nal buffer (bits/position buffer) (bits/position buffer)))
  ([buffer pos origpos]
     (if (or (= (bits/next-bytes buffer 3 pos) 0)
             (= (bits/next-bytes buffer 3 pos) 0))
       (- pos origpos)
       (recur buffer (+ pos 1) origpos))))

(defn log [a m]
  (println m)
  a)

(defn parse-rbsp [ntype rbsp]
  (condp = ntype
    6 (sei/parse-sei-rbsp rbsp)
    7 (params/parse-seq-params rbsp)
    [ntype rbsp]))

(defn parse-nal [buffer size]
  (let [byte (bits/getb buffer)
        zero-bit (bit-and 0x80 byte)
        refidc (bit-shift-right (bit-and 0x60 byte) 5)
        unittype (bit-and 0x1F byte)
        rbsp (ByteBuffer/allocateDirect size)]
    (if (or (= unittype 14)
            (= unittype 20)
            (= unittype 21))
      (throw (Exception. (str "unsupported nal type " unittype)))
      (loop [i 1]
        (cond
         (and (< (+ i 2) size)
              (= (bits/next-bytes buffer 3) 3))
          (do
            (doto rbsp
              (bits/putb (bits/getb buffer))
              (bits/putb (bits/getb buffer)))
            (discard-bytes buffer 1 3)
            (recur (+ i 1)))

          (< i size)
          (do
            (bits/putb rbsp (bits/getb buffer))
            (recur (+ i 1)))

          :else
          (parse-rbsp unittype (.rewind rbsp)))))))

(defn extract-nal [buffer]
  (let [buffer-at-start-of-nal
        (-> buffer
            move-to-start-of-nal
            (discard-bytes 1 0)
            (discard-bytes 3 1)
            )
        size-of-nal (get-size-of-nal buffer)
        ]
    (parse-nal buffer size-of-nal)))

(println "\n\n")

(let [a (afile)]
  (println "----")
  (println (extract-nal a))
  (println (extract-nal a))
  (println (extract-nal a))
  (println (extract-nal a))
  (println (extract-nal a))
  (println (extract-nal a))
  (println (extract-nal a))
  (println (extract-nal a))
  (println (extract-nal a))
  (println (extract-nal a))
  (println "----"))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x ""))
