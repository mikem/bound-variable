(ns bound-variable.core)

;;; some constants
(def *word-size* 32)
(def *opcode-size* 4)
(def *get-opcode-shift-amount* (- *word-size* *opcode-size*))
(def *integer-modulus* (Math/pow 2 32))

;;; registers
(def *bits-per-register* 3)
(def *register-offsets* {:a (* *bits-per-register* 2)
                         :b (* *bits-per-register* 1)
                         :c (* *bits-per-register* 0)})
(def *register-mask* 0x7) ; binary 0000 0000 0000 0111
(def *load-register-offset* (- *word-size* *opcode-size* *bits-per-register*))
(def *load-value-mask* 0x01ffffff) ; 0000 0001 1111 1111 1111 1111 1111 1111

;;; eight registers, initialized to 0
(def *registers* (ref [0 0 0 0 0 0 0 0]))
(def *arrays* (ref {}))

(defn get-opcode [instruction]
  (bit-shift-right instruction *get-opcode-shift-amount*))

(defn get-register [register instruction]
  (bit-and
    (bit-shift-right instruction (*register-offsets* register))
    *register-mask*))

(defn get-register-value [register instruction]
  (*registers* (get-register register instruction)))

(defn set-register-value [register value]
  (dosync (alter *registers* assoc register value)))

(defn allocate-array [array size]
  (dosync (alter *arrays* assoc array (vec (replicate size 0)))))

(defn abandon-array [array]
  (dosync (alter *arrays* dissoc array)))

(defn get-array-value [array index]
  ((@*arrays* array) index))

(defn set-array-value [array index value]
  (dosync (alter *arrays* assoc array (assoc (@*arrays* array) index value))))

(defn print-char [c]
  (print c))

(defn read-char []
  (.read *in*))

(defn abort []
  (do
    (println "Aborting!")
    (System/exit 1)))

(defmulti execute-instruction get-opcode)

; Operator 0: move contents of B to A if contents of C is non-zero
(defmethod execute-instruction 0x0 [instruction]
  (let [ra (get-register :a instruction)
        rbv (get-register-value :b instruction)
        rcv (get-register-value :c instruction)]
    (when-not (zero? rcv)
      (set-register-value ra rbv))))

; Operator 1: array access
(defmethod execute-instruction 0x1 [instruction]
  (let [ra (get-register :a instruction)
        rbv (get-register-value :b instruction)
        rcv (get-register-value :c instruction)]
    (set-register-value ra (get-array-value rbv rcv))))

; Operator 2: array amendment
(defmethod execute-instruction 0x2 [instruction]
  (let [rav (get-register-value :a instruction)
        rbv (get-register-value :b instruction)
        rcv (get-register-value :c instruction)]
    (set-array-value rav rbv rcv)))

(defn execute-arithmetic-instruction [instruction op]
  (let [ra (get-register :a instruction)
        rbv (get-register-value :b instruction)
        rcv (get-register-value :c instruction)
        result (rem (op rbv rcv) *integer-modulus*)]
    (set-register-value ra result)))

; Operator 3: A = (B + C) % 2^32
(defmethod execute-instruction 0x3 [instruction]
  (execute-arithmetic-instruction instruction +))

; Operator 4: A = (B * C) % 2^32
(defmethod execute-instruction 0x4 [instruction]
  (execute-arithmetic-instruction instruction *))

; Operator 5: A = (B / C)
(defmethod execute-instruction 0x5 [instruction]
  (let [ra (get-register :a instruction)
        rbv (get-register-value :b instruction)
        rcv (get-register-value :c instruction)
        dividend (int (/ rbv rcv))]
    (set-register-value ra dividend)))

; Operator 6: A = ~(B & C)
(defmethod execute-instruction 0x6 [instruction]
  (let [ra (get-register :a instruction)
        rbv (get-register-value :b instruction)
        rcv (get-register-value :c instruction)
        result (bit-not (bit-and rbv rcv))]
    (set-register-value ra result)))

; Operator 7: HALT
(defmethod execute-instruction 0x7 [instruction]
  (abort))

; Operator 8: array allocation
(defmethod execute-instruction 0x8 [instruction]
  (let [array (get-register-value :b instruction)
        size (get-register-value :c instruction)]
    (allocate-array array size)))

; Operator 9: array abandonment
(defmethod execute-instruction 0x9 [instruction]
  (abandon-array (get-register-value :c instruction)))

; Operator 10: output
(defmethod execute-instruction 0xa [instruction]
  (print-char (get-register-value :c instruction)))

; Operator 11: input
(defmethod execute-instruction 0xb [instruction]
  (set-register-value (get-register :c instruction) (read-char)))

; Operator 13: A <- value
(defmethod execute-instruction 0xd [instruction]
  (let [ra (bit-and
             (bit-shift-right instruction *load-register-offset*)
             *register-mask*)
        value (bit-and instruction *load-value-mask*)]
    (set-register-value ra value)))

(defn get-int-from-byte-quad [byte-quad]
  (loop [shift-amount 24
         quad byte-quad
         result 0]
    (if-not quad
      result
      (recur (- shift-amount 8)
             (next quad)
             (bit-or result (bit-shift-left (bit-and 0xff (first quad)) shift-amount))))))

(defn get-int-vector-from-byte-array [arr]
  (loop [partitioned-arr (partition 4 (map
                                        #(byte (if (bit-test % 7)
                                                 (bit-or % -128)
                                                 (bit-and % 127)))
                                        arr))
         int-vec []]
    (if-not partitioned-arr
      int-vec
      (recur (next partitioned-arr)
             (conj int-vec (get-int-from-byte-quad (first partitioned-arr)))))))


(defn initialize [input-filename] 0)
