(ns h264.sei
  (:require [h264.bits :as bits]))

(set! *warn-on-reflection* true)

(defn- read-long
  ([buffer] (read-long buffer 0))
  ([buffer n]
     (let [nextb (bit-and 0xFF (bits/getb buffer))]
       (if (= nextb 0xFF)
         (recur buffer (+ n 255))
         (+ n nextb)))))

(defn- user-data-unregistered [buffer payload-size]
  (let [uuid-pfx (Long/toHexString (bits/getl buffer))
        uuid-sfx (Long/toHexString (bits/getl buffer))
        uuid (str uuid-pfx uuid-sfx)
        payload (amap (byte-array (- payload-size 16)) idx ret (bits/getb buffer))
        ]
    [:user-data uuid payload]))

(defn- parse-sei-payload [buffer payload-type payload-size]
  (condp = payload-type
    5 (user-data-unregistered buffer payload-size)
    []))

(defn- parse-sei-message [buffer]
  (let [payload-type (read-long buffer)
        payload-size (read-long buffer)]
    (parse-sei-payload buffer payload-type payload-size)))

(defn parse-sei-rbsp
  ([buffer] (parse-sei-rbsp buffer []))
  ([buffer seis]
     (if (bits/empty buffer)
       seis
       (let [msg (parse-sei-message buffer)]
         (parse-sei-rbsp buffer (cons msg seis))))))
