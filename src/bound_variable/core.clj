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

(defmulti execute-instruction get-opcode)

; Operator 0: move contents of B to A if contents of C is non-zero
(defmethod execute-instruction 0x0 [instruction]
  (let [ra (get-register :a instruction)
        rbv (get-register-value :b instruction)
        rcv (get-register-value :c instruction)]
    (when-not (zero? rcv)
      (dosync (alter *registers* assoc ra rbv)))))

(defn execute-arithmetic-instruction [instruction op]
  (let [ra (get-register :a instruction)
        rbv (get-register-value :b instruction)
        rcv (get-register-value :c instruction)
        result (rem (op rbv rcv) *integer-modulus*)]
    (dosync (alter *registers* assoc ra result))))

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
    (dosync (alter *registers* assoc ra dividend))))

; Operator 8: array allocation
(defmethod execute-instruction 0x8 [instruction]
  (let [array (get-register-value :b instruction)
        size (get-register-value :c instruction)]
    (dosync (alter *arrays* assoc array (vec (replicate size 0))))))

; Operator 13: A <- value
(defmethod execute-instruction 0xd [instruction]
  (let [ra (bit-and
             (bit-shift-right instruction *load-register-offset*)
             *register-mask*)
        value (bit-and instruction *load-value-mask*)]
    (dosync (alter *registers* assoc ra value))))
