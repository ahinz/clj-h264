(ns h264.params
  (:require [h264.bits :as bits]))

(defn parse-seq-params [buffer]
  (let [profile-idc (bits/getb buffer)
        constraint-set-flags (bits/getb buffer)
        constraint-set0-flag (= 0x80 (bit-and 0x80 constraint-set-flags))
        constraint-set1-flag (= 0x40 (bit-and 0x40 constraint-set-flags))
        constraint-set2-flag (= 0x20 (bit-and 0x20 constraint-set-flags))
        constraint-set3-flag (= 0x10 (bit-and 0x10 constraint-set-flags))
        constraint-set4-flag (= 0x08 (bit-and 0x08 constraint-set-flags))
        constraint-set5-flag (= 0x04 (bit-and 0x04 constraint-set-flags))
        level-idc (bits/getb buffer)
        bs (bits/create-bit-stream buffer)
        [seq-parameter-set-id bs] (bits/ue bs)
        [log2-max-frame-num-minus4 bs] (bits/ue bs)
        [pic-order-cnt-type bs] (bits/ue bs)
        max-frame (bit-shift-left 1 (+ 4 log2-max-frame-num-minus4))
        ]

    ;; Can skip chroma format idc stuff (page 42 of spec, 7.3.2.1.1)
    (if (not (= profile-idc 66))
      (throw (Exception. "only baseline profile supported")))

    {:profile-idc profile-idc
     :level-idc level-idc
     :max-frame max-frame
     :pic-order-cnt-type pic-order-cnt-type
     :seq-parameter-set-id seq-parameter-set-id
     :constraint-set0-flag constraint-set0-flag
     :constraint-set1-flag constraint-set1-flag
     :constraint-set2-flag constraint-set2-flag
     :constraint-set3-flag constraint-set3-flag
     :constraint-set4-flag constraint-set4-flag
     :constraint-set5-flag constraint-set5-flag}))
