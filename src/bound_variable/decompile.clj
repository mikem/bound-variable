(ns bound-variable.decompile
  (:use [bound-variable.common]))

(defn get-standard-registers [instruction]
  (vec (map #(get-register % instruction) [:a :b :c])))

(defmulti decompile-instruction get-opcode)

(defmethod decompile-instruction 0x0 [instruction]
  (let [[ra rb rc] (get-standard-registers instruction)]
    (format "0x%08x cmv r%d, r%d, r%d" instruction ra rb rc)))

(defmethod decompile-instruction 0x1 [instruction]
  (let [[ra rb rc] (get-standard-registers instruction)]
    (format "0x%08x aix r%d, r%d, r%d" instruction ra rb rc)))

(defmethod decompile-instruction 0x2 [instruction]
  (let [[ra rb rc] (get-standard-registers instruction)]
    (format "0x%08x arm r%d, r%d, r%d" instruction ra rb rc)))

(defmethod decompile-instruction 0x3 [instruction]
  (let [[ra rb rc] (get-standard-registers instruction)]
    (format "0x%08x add r%d, r%d, r%d" instruction ra rb rc)))

(defmethod decompile-instruction 0x4 [instruction]
  (let [[ra rb rc] (get-standard-registers instruction)]
    (format "0x%08x mul r%d, r%d, r%d" instruction ra rb rc)))

(defmethod decompile-instruction 0x5 [instruction]
  (let [[ra rb rc] (get-standard-registers instruction)]
    (format "0x%08x div r%d, r%d, r%d" instruction ra rb rc)))

(defmethod decompile-instruction 0x6 [instruction]
  (let [[ra rb rc] (get-standard-registers instruction)]
    (format "0x%08x nta r%d, r%d, r%d" instruction ra rb rc)))

(defmethod decompile-instruction 0x7 [instruction]
  (format "0x%08x hlt" instruction))

(defmethod decompile-instruction 0x8 [instruction]
  (let [[ra rb rc] (get-standard-registers instruction)]
    (format "0x%08x alc r%d, r%d" instruction rb rc)))

(defmethod decompile-instruction 0x9 [instruction]
  (let [[ra rb rc] (get-standard-registers instruction)]
    (format "0x%08x abd r%d" instruction rc)))

(defmethod decompile-instruction 0xa [instruction]
  (let [[ra rb rc] (get-standard-registers instruction)]
    (format "0x%08x out r%d" instruction rc)))

(defmethod decompile-instruction 0xb [instruction]
  (let [[ra rb rc] (get-standard-registers instruction)]
    (format "0x%08x ipt r%d" instruction rc)))

(defmethod decompile-instruction 0xc [instruction]
  (let [[ra rb rc] (get-standard-registers instruction)]
    (format "0x%08x ldp r%d, r%d" instruction rb rc)))

(defmethod decompile-instruction 0xd [instruction]
  (let [ra (bit-and (bit-shift-right instruction *load-register-offset*) *register-mask*)
        value (bit-and instruction *load-value-mask*)]
    (format "0x%08x lod r%d, %d" instruction ra value)))

(defmethod decompile-instruction :default [instruction]
  (format "unknown 0x%08x" instruction))
