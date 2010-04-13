(ns bound-variable.decompile
  (:use [bound-variable.common]))

(defn get-standard-registers [instruction]
  (vec (map #(get-register % instruction) [:a :b :c])))

(defmulti decompile-instruction get-opcode)

(defmethod decompile-instruction 0x0 [instruction]
  (let [[ra rb rc] (get-standard-registers instruction)]
    (str "cmv r" ra ", r" rb ", r" rc)))

(defmethod decompile-instruction 0x1 [instruction]
  (let [[ra rb rc] (get-standard-registers instruction)]
    (str "aix r" ra ", r" rb ", r" rc)))

(defmethod decompile-instruction 0x2 [instruction]
  (let [[ra rb rc] (get-standard-registers instruction)]
    (str "arm r" ra ", r" rb ", r" rc)))

(defmethod decompile-instruction 0x3 [instruction]
  (let [[ra rb rc] (get-standard-registers instruction)]
    (str "add r" ra ", r" rb ", r" rc)))

(defmethod decompile-instruction 0x4 [instruction]
  (let [[ra rb rc] (get-standard-registers instruction)]
    (str "mul r" ra ", r" rb ", r" rc)))

(defmethod decompile-instruction 0x5 [instruction]
  (let [[ra rb rc] (get-standard-registers instruction)]
    (str "div r" ra ", r" rb ", r" rc)))

(defmethod decompile-instruction 0x6 [instruction]
  (let [[ra rb rc] (get-standard-registers instruction)]
    (str "nta r" ra ", r" rb ", r" rc)))

(defmethod decompile-instruction 0x7 [instruction]
  (str "hlt"))

(defmethod decompile-instruction 0x8 [instruction]
  (let [[ra rb rc] (get-standard-registers instruction)]
    (str "alc r" rb ", r" rc)))

(defmethod decompile-instruction 0x9 [instruction]
  (let [[ra rb rc] (get-standard-registers instruction)]
    (str "abd r" rc)))

(defmethod decompile-instruction 0xa [instruction]
  (let [[ra rb rc] (get-standard-registers instruction)]
    (str "out r" rc)))

(defmethod decompile-instruction 0xb [instruction]
  (let [[ra rb rc] (get-standard-registers instruction)]
    (str "ipt r" rc)))

(defmethod decompile-instruction 0xc [instruction]
  (let [[ra rb rc] (get-standard-registers instruction)]
    (str "ldp r" rb ", r" rc)))

(defmethod decompile-instruction 0xd [instruction]
  (let [ra (bit-and (bit-shift-right instruction *load-register-offset*) *register-mask*)
        value (bit-and instruction *load-value-mask*)]
    (str "lod r" ra ", " value)))
