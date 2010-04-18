(ns bound-variable.core
  (:gen-class)
  (:use [bound-variable.common]
        [bound-variable.decompile]
        [clojure.contrib.io :only (to-byte-array)]
        [clojure.contrib.command-line :only (with-command-line)]))

;;; eight registers, initialized to 0
(def *registers* (ref [0 0 0 0 0 0 0 0]))
(def *arrays* (ref {}))

;;; the program counter
(def *pc* (atom 0))

(defn get-register-value [register instruction]
  (*registers* (get-register register instruction)))

(defn set-register-value [register value]
  (dosync (alter *registers* assoc register value)))

(defn set-array [array-idx src-array]
  (dosync (alter *arrays* assoc array-idx src-array)))

(defn allocate-array [size]
  (let [array-idx (first
                    (filter
                      #(not (contains? @*arrays* %))
                      (take java.lang.Integer/MAX_VALUE (iterate inc 1))))]
    (set-array array-idx (vec (replicate size 0)))
    array-idx))

(defn abandon-array [array-idx]
  (dosync (alter *arrays* dissoc array-idx)))

(defn get-array-value [array-idx index]
  ((@*arrays* array-idx) index))

(defn set-array-value [array-idx index value]
  (dosync (alter *arrays* assoc array-idx (assoc (@*arrays* array-idx) index value))))

(defn print-char [c]
  (print (char c))
  (flush))

(defn read-char []
  (.read *in*))

(defn abort []
  (do
    (println "\nAborting!")
    (System/exit 1)))

(defn print-instruction-info [instruction]
  (let [hex-instruction (Integer/toHexString (get-array-value 0 @*pc*))
        ra (get-register :a instruction)
        rb (get-register :b instruction)
        rc (get-register :c instruction)
        rav (get-register-value :a instruction)
        rbv (get-register-value :b instruction)
        rcv (get-register-value :c instruction)]
    (println (str "Executing " (decompile-instruction instruction)))
    (println (format "Ra: %1$d -> %2$10d [0x%2$08x] Rb: %3$d -> %4$10d [0x%4$08x] Rc: %5$d -> %6$10d [0x%6$08x]"
                     ra rav rb rbv rc rcv))
    (println (str @*registers*))
    (doseq [arr-key (keys @*arrays*)]
      (println "array" arr-key "length" (count (@*arrays* arr-key))))
    (println (str "PC: " @*pc* "\n"))))

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
        result (convert-to-int (op rbv rcv))]
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
        rbv (bit-and 0xffffffff (get-register-value :b instruction))
        rcv (bit-and 0xffffffff (get-register-value :c instruction))
        dividend (int (quot rbv rcv))]
    (set-register-value ra dividend)))

; Operator 6: A = ~(B & C)
(defmethod execute-instruction 0x6 [instruction]
  (let [ra (get-register :a instruction)
        rbv (convert-to-int (get-register-value :b instruction))
        rcv (convert-to-int (get-register-value :c instruction))
        result (bit-not (bit-and rbv rcv))]
    (set-register-value ra result)))

; Operator 7: HALT
(defmethod execute-instruction 0x7 [instruction]
  (abort))

; Operator 8: array allocation
(defmethod execute-instruction 0x8 [instruction]
  (let [size (get-register-value :c instruction)
        rb (get-register :b instruction)]
    (set-register-value rb (allocate-array size))))

; Operator 9: array abandonment
(defmethod execute-instruction 0x9 [instruction]
  (abandon-array (get-register-value :c instruction)))

; Operator 10: output
(defmethod execute-instruction 0xa [instruction]
  (print-char (get-register-value :c instruction)))

; Operator 11: input
(defmethod execute-instruction 0xb [instruction]
  (set-register-value (get-register :c instruction) (read-char)))

; Operator 12: load program
(defmethod execute-instruction 0xc [instruction]
  (let [rbv (get-register-value :b instruction)
        rcv (get-register-value :c instruction)]
    (set-array 0 (@*arrays* rbv))
    ; we decrement the value in register C as it's incremented again after this
    ; instruction is executed
    (reset! *pc* (dec rcv))))

; Operator 13: A <- value
(defmethod execute-instruction 0xd [instruction]
  (let [ra (bit-and
             (bit-shift-right instruction *load-register-offset*)
             *register-mask*)
        value (bit-and (convert-to-int instruction) *load-value-mask*)]
    (set-register-value ra value)))

(defn get-int-from-byte-quad [[b0 b1 b2 b3]]
  (->                       (bit-and 0xff b3)
    (bit-or (bit-shift-left (bit-and 0xff b2) 8))
    (bit-or (bit-shift-left (bit-and 0xff b1) 16))
    (bit-or (bit-shift-left (bit-and 0xff b0) 24))))

; Unknown operator, abort
(defmethod execute-instruction :default [instruction]
  (abort))

(defn get-int-vector-from-byte-array [arr]
  (loop [partitioned-arr (partition 4 (map convert-to-byte arr))
         int-vec []]
    (if-not partitioned-arr
      int-vec
      (recur (next partitioned-arr)
             (conj int-vec (get-int-from-byte-quad (first partitioned-arr)))))))

(defn initialize [input-filename]
  (dosync (ref-set *registers* [0 0 0 0 0 0 0 0]))
  (reset! *pc* 0)
  (->> input-filename
       (java.io.File.)
       (to-byte-array)
       (get-int-vector-from-byte-array)
       (set-array 0)))

(defn run
  ([] (run false))
  ([verbose?]
  (let [instruction (get-array-value 0 @*pc*)]
    (when verbose?
      (print-instruction-info instruction))
    (execute-instruction instruction)
    (swap! *pc* inc)
    (recur verbose?))))

(defn print-assembly []
  (let [pc (atom 0)]
    (doseq [instruction (@*arrays* 0)]
      (println (format "%6d %s" @pc (decompile-instruction instruction)))
      (swap! pc inc))))

(defn -main [& args]
  (with-command-line args
    "Run Universal Machine"
    [[input-filename i "program scroll (input file)"]
     [decompile? d "prints assembly instead of executing program"]
     [verbose? v "dump info for each instruction"]]
    (initialize input-filename)
    (if decompile?
      (print-assembly)
      (run verbose?))))
