(ns bound-variable.common)

;;; some constants
(def *word-size* 32)
(def *opcode-size* 4)
(def *get-opcode-shift-amount* (- *word-size* *opcode-size*))

;;; registers
(def *bits-per-register* 3)
(def *register-offsets* {:a (* *bits-per-register* 2)
                         :b (* *bits-per-register* 1)
                         :c (* *bits-per-register* 0)})
(def *register-mask* 0x7) ; binary 0000 0000 0000 0111
(def *load-register-offset* (- *word-size* *opcode-size* *bits-per-register*))
(def *load-value-mask* 0x01ffffff) ; 0000 0001 1111 1111 1111 1111 1111 1111

(defn convert-to-byte [value]
  (byte (if (bit-test value 7)
          (bit-or value -128)
          (bit-and value 127))))

(defn convert-to-int [value]
  (int (if (bit-test value 31)
          (bit-or value -2147483648)
          (bit-and value 2147483647))))

(defn get-opcode [instruction]
  (bit-shift-right instruction *get-opcode-shift-amount*))

(defn get-register [register instruction]
  (bit-and
    (bit-shift-right instruction (*register-offsets* register))
    *register-mask*))
