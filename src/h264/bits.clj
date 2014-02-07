(ns h264.bits
  (:import [java.nio ByteBuffer]
           [java.nio.channels FileChannel$MapMode]
           [java.io FileInputStream File]))

(set! *warn-on-reflection* true)

(defn getl [^ByteBuffer buffer]
  (.getLong buffer))

(defn putb [^ByteBuffer buffer ^Byte byte]
  (.put buffer byte))

(defn getb
  ([^ByteBuffer buffer] (.get buffer))
  ([^ByteBuffer buffer ^long n] (.get buffer n)))

(defn position
  ([^ByteBuffer buffer] (.position buffer))
  ([^ByteBuffer buffer ^long n] (.position buffer n)))

(defn empty [^ByteBuffer buffer]
  (= 0 (.remaining buffer)))

(defn next-bytes
  ([buffer bytes] (next-bytes buffer bytes (position buffer)))
  ([buffer bytes pos] (next-bytes buffer bytes pos 0))
  ([buffer bytes pos val]
     (if (<= bytes 0)
       val
       (recur
        buffer
        (- bytes 1)
        (+ pos 1)
        (bit-or (bit-shift-left val 8) (getb buffer pos))))))

(defn skip [buffer bytes]
  (position buffer (+ (position buffer) bytes)))

(defn create-bit-stream [buffer]
  {:buffer buffer :byte 0 :idx 8})

(defn next-bit [bitstream]
  (let [byte (:byte bitstream)
        idx (:idx bitstream)]
    (if (< idx 8)
      (let [bit (= 0x80 (bit-and 0x80 byte))]
        [bit (assoc bitstream :idx (+ 1 idx) :byte (bit-shift-left byte 1))])
      (next-bit (assoc bitstream :idx 0 :byte (getb (:buffer bitstream)))))))

(defn- leading-zeros [bitstream n]
  (let [[bit bitstream] (next-bit bitstream)]
    (if bit [n bitstream] (recur bitstream (+ n 1)))))

(defn- read-bits [bitstream n v]
  (if (<= n 0) [v bitstream]
      (let [[bit bitstream] (next-bit bitstream)
            bit (if bit 1 0)
            v (bit-or (bit-shift-left v 1) bit)]
        (recur bitstream (- n 1) v))))

(defn ue [bitstream]
  (let [[leading-bits bitstream] (leading-zeros bitstream 0)
        [bits bitstream] (read-bits bitstream leading-bits 0)]
    [(+ (bit-shift-left 1 leading-bits)
        -1
        bits)
     bitstream]))
