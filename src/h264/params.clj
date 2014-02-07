(ns h264.params
  (:require [h264.bits :as bits]))

(defn parse-seq-params [buffer]
  (let [profile-idc (bits/getb buffer)
        constraint_set_flags (bits/getb buffer)
        constraint_set0_flag (= 0x80 (bit-and 0x80 constraint_set_flags))
        constraint_set1_flag (= 0x40 (bit-and 0x40 constraint_set_flags))
        constraint_set2_flag (= 0x20 (bit-and 0x20 constraint_set_flags))
        constraint_set3_flag (= 0x10 (bit-and 0x10 constraint_set_flags))
        constraint_set4_flag (= 0x08 (bit-and 0x08 constraint_set_flags))
        constraint_set5_flag (= 0x04 (bit-and 0x04 constraint_set_flags))
        level-idc (bits/getb buffer)
        seq-parameter-set-id ()]
    {:profile-idc profile-idc
     :level-idc level-idc
     :constraint_set0_flag constraint_set0_flag
     :constraint_set1_flag constraint_set1_flag
     :constraint_set2_flag constraint_set2_flag
     :constraint_set3_flag constraint_set3_flag
     :constraint_set4_flag constraint_set4_flag
     :constraint_set5_flag constraint_set5_flag}))
