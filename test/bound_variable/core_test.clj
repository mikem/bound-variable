(ns bound-variable.core-test
  (:use [bound-variable.core] :reload-all)
  (:use [bound-variable.common] :reload-all)
  (:use [com.stuartsierra.lazytest :only (is are given spec defcontext find-spec)])
  (:use [com.stuartsierra.lazytest.report :only (spec-report)])
  (:use [com.stuartsierra.lazytest.color :only (set-colorize)]))

;(set-colorize false)

;;; contexts
(defcontext setup-registers []
  (dosync (ref-set *registers* [1 2 3 0 4 2147483647 777 0]))
  :after [_]
  (dosync (ref-set *registers* [0 0 0 0 0 0 0 0])))

(defcontext setup-arrays []
  (dosync (ref-set *arrays* {3 [5 6 7 8]
                             1 [22 33 44]}))
  :after [_]
  (dosync (ref-set *arrays* {})))

(defcontext create-temp-file []
  (java.io.File/createTempFile "bound-variable-" "-test")
  :after [f]
  (.delete f))

;;; helper functions & data
(defn exec-and-fetch-register [instruction register]
  (execute-instruction instruction)
  (*registers* register))

(defn exec-and-fetch-from-array [instruction array index]
  (execute-instruction instruction)
  ((@*arrays* array) index))

(defn write-binary-file [filename arr]
  (with-open [f (java.io.FileOutputStream. filename)]
    (.write f arr)))

(def sample-input-array (into-array [0x01 0x23 0x45 0x67
                                     0x12 0x34 0x56 0x78
                                     0x23 0x45 0x67 0x89
                                     0x34 0x56 0x78 0x9a
                                     0x45 0x67 0x89 0xab
                                     0x56 0x78 0x9a 0xbc
                                     0x67 0x89 0xab 0xcd
                                     0x78 0x9a 0xbc 0xde
                                     0x89 0xab 0xcd 0xef
                                     0x9a 0xbc 0xde 0xf0
                                     0xab 0xcd 0xef 0x01
                                     0xbc 0xde 0xf0 0x12
                                     0xcd 0xef 0x01 0x23]))

(def sample-byte-array (byte-array (map convert-to-byte sample-input-array)))

(def small-sample-input-array (into-array [0x02 0x46 0x8a 0xce]))

(def small-sample-byte-array (byte-array (map convert-to-byte small-sample-input-array)))

;;; specs begin
(spec test-get-opcode
  (are [instruction opcode] (= (get-opcode instruction) opcode)
    0x01234567 0x0
    0x12345678 0x1
    0x23456789 0x2
    0x3456789a 0x3
    0x456789ab 0x4
    0x56789abc 0x5
    0x6789abcd 0x6
    0x789abcde 0x7
    0x89abcdef 0x8
    0x9abcdef0 0x9
    0xabcdef01 0xa
    0xbcdef012 0xb
    0xcdef0123 0xc))

(spec test-get-register
  (are [instruction register value] (= (get-register register instruction) value)
    0x000002be :a 0x2 ; % 0000 0010 1011 1110
                      ;           ^ ^^
    0x000002be :b 0x7 ; % 0000 0010 1011 1110
                      ;               ^^ ^
    0x000002be :c 0x6 ; % 0000 0010 1011 1110
                      ;                   ^^^
    0x0000031a :a 0x4 ; % 0000 0011 0001 1010
                      ;           ^ ^^
    0x00000098 :b 0x3 ; % 0000 0000 1001 1000
                      ;               ^^ ^
    0x0000000f :c 0x7 ; % 0000 0000 0000 1111
                      ;                   ^^^
    0x0000011a :a 0x4 ; % 0000 0001 0001 1010
                      ;           ^ ^^
    0x00000018 :b 0x3 ; % 0000 0000 0001 1000
                      ;               ^^ ^
    0x00000007 :c 0x7 ; % 0000 0000 0000 0111
                      ;                   ^^^
    0x00000000 :a 0x0
    0x00000000 :b 0x0
    0x00000000 :c 0x0))

(spec test-get-register-value
  (given [_ setup-registers]
    (= 2 (get-register-value :a 0x00000042))   ; % 0000 0000 0100 0010
    (= 1 (get-register-value :b 0x00000042))   ; % 0000 0000 0100 0010
    (= 3 (get-register-value :c 0x00000042)))) ; % 0000 0000 0100 0010

(spec test-exec-operator-0 "Tests instructions with opcode 0"
  (spec test-exec-operator-0-0
    (given [_ setup-registers]
      "Move contents of B to A because contents of C is non-zero"
      (= 0x3 (exec-and-fetch-register 0x000001d0 7))))  ; % 0000 0001 1101 0000
  (spec test-exec-operator-0-1
    (given [_ setup-registers]
      "Do nothing because contents of C is zero"
      (= 0x0 (exec-and-fetch-register 0x000001d3 7))))) ; % 0000 0001 1101 0011

(spec test-exec-operator-1
  (given [_ setup-registers
          _ setup-arrays]
    "8 = ((@*arrays* 3) 3)"
    (= 8 (exec-and-fetch-register 0x100001d2 7)))   ; % 0000 0001 1101 0010
  (given [_ setup-registers
          _ setup-arrays]
    "44 = ((@*arrays* 1) 2)"
    (= 44 (exec-and-fetch-register 0x10000181 6)))) ; % 0000 0001 1000 0001

(spec test-exec-operator-2
  (given [_ setup-registers
          _ setup-arrays]
    "A[B] = C"
    (= 777 (exec-and-fetch-from-array 0x2000008e 3 2)))) ; % 0000 0000 1000 1110

(spec test-exec-operator-3
  (given [_ setup-registers]
    "4 = 3 + 1 (A = B + C)"
    (= 4 (exec-and-fetch-register 0x300000d0 3)))  ; % 0000 0000 1101 0000
  (given [_ setup-registers]
    "4 = 2 + 2 (A = B + C)"
    (= 4 (exec-and-fetch-register 0x30000109 4)))  ; % 0000 0001 0000 1001
  (given [_ setup-registers]
    "5 = 2 + 3 (A = B + C)"
    (= 5 (exec-and-fetch-register 0x3000014a 5)))  ; % 0000 0001 0100 1010
  (given [_ setup-registers]
    "-2147483648 = 2147483647 + 1 (A = B + C)"
    (= -2147483648 (exec-and-fetch-register 0x300001a8 6)))) ; % 0000 0001 1010 1000

(spec test-exec-operator-4
  (given [_ setup-registers]
    "6 = 3 * 2 (A = B * C)"
    (= 6 (exec-and-fetch-register 0x400001d1 7))) ; % 0000 0001 1101 0001
  (given [_ setup-registers]
    "9 = 3 * 3 (A = B * C)"
    (= 9 (exec-and-fetch-register 0x40000092 2))) ; % 0000 0000 1001 0010
  (given [_ setup-registers]
    "-2 = 2147483647 * 2 (A = B * C)"
    (= -2 (exec-and-fetch-register 0x400000e9 3)))) ; % 0000 0000 1110 1001

(spec test-exec-operator-5
  (given [_ setup-registers]
    "2 = 4 / 2 (A = B / C)"
    (= 2 (exec-and-fetch-register 0x500001e1 7)))  ; % 0000 0001 1110 0001
  (given [_ setup-registers]
    "1 = 4 / 3 (A = B / C)"
    (= 1 (exec-and-fetch-register 0x500001e2 7)))) ; % 0000 0001 1110 0010

; TODO: test divide by zero

(spec test-exec-operator-6
  (given [_ setup-registers]
    "A = ~(B & C)"
    (= -1 (exec-and-fetch-register 0x60000188 6)))) ; % 0000 0001 1000 1000

(spec test-exec-operator-7
  "Operator 7 is the HALT instruction; call (abort)"
  (is
    (= true
       (let [called (atom false)]
         (binding [bound-variable.core/abort (fn [] (reset! called true))]
           (execute-instruction 0x70000000))
         @called))))

(spec test-exec-operator-8
  (given [_ setup-registers
          _ setup-arrays]
    "allocate array 4 of size 3"
    (= [0 0 0]
       (do (execute-instruction 0x80000022) ; % 0000 0000 0010 0010
           (@*arrays* 4))))
  (given [_ setup-registers
          _ setup-arrays]
    "allocate array 3 of size 4"
    (= [0 0 0 0]
       (do (execute-instruction 0x80000014) ; % 0000 0000 0001 0100
           (@*arrays* 3)))))

; TODO: test allocating an array that already exists

(spec test-exec-operator-9
  (given [_ setup-registers]
    (= false
       (do (dosync (alter *arrays* assoc 2 [0 1 2 3 4]))
           (execute-instruction 0x90000001)
           (contains? @*arrays* 2)))))

; TODO: - abandon the 0 array
;       - abandond an inactive array

(spec test-exec-operator-10
  (given [_ setup-registers]
    (= true
       (let [called (atom false)]
         (binding [bound-variable.core/print-char (fn [_] (reset! called true))]
           (execute-instruction 0xa0000002))
         @called))))

(spec test-exec-operator-11
  (given [_ setup-registers]
    (= 100
       (binding [bound-variable.core/read-char (fn [] (int (.charValue \d)))]
         (exec-and-fetch-register 0xb0000003 3)))))  ; % 0000 0000 0000 0011

(spec test-exec-operator-12
  (given [_ setup-registers
          _ setup-arrays]
    (do (execute-instruction 0xc0000011) ; % 0000 0000 0001 0001
        (and
          ; array identified by register B is copied to array 0
          (= [5 6 7 8]
             (@*arrays* 0))
          ; program counter contains value in register C
          ; NOTE: the expected program counter is one less than the value
          ; contained in register C, as it's incremented immediately after the
          ; instruction is executed in the main program.
          (= 1 @*pc*)))))

(spec test-exec-operator-13
  (given [_ setup-registers]
    "A <- 6"
    (= 6 (exec-and-fetch-register 0xde000006 7))) ; % 1101 1110 ...
  (given [_ setup-registers]
    "A <- 33554431"
    (= 33554431 (exec-and-fetch-register 0xdfffffff 7))) ; % 1101 1111 ...
  (is
    (= java.lang.Integer
       (type (exec-and-fetch-register 0xdc000000 6))))) ; % 1101 1100 ... This one's from real code

(spec test-get-int-vector-from-byte-array
  (is
    (= [0x01234567 0x12345678 0x23456789 0x3456789a 0x456789ab
        0x56789abc 0x6789abcd 0x789abcde 0x89abcdef 0x9abcdef0
        0xabcdef01 0xbcdef012 0xcdef0123]
       (get-int-vector-from-byte-array sample-input-array))))

(spec test-initialize
  (given [f create-temp-file]
    "Set 0 array to contents of file, v1"
    (= [0x01234567 0x12345678 0x23456789 0x3456789a 0x456789ab
        0x56789abc 0x6789abcd 0x789abcde 0x89abcdef 0x9abcdef0
        0xabcdef01 0xbcdef012 0xcdef0123]
       (do (write-binary-file f sample-byte-array)
           (initialize (.getCanonicalPath f))
           (@*arrays* 0)))
    "Set 0 array to contents of file, v2"
    (= [0x02468ace]
       (do (write-binary-file f small-sample-byte-array)
           (initialize (.getCanonicalPath f))
           (@*arrays* 0)))
    "Ensure all registers preset with 0, v1"
    (= [0 0 0 0 0 0 0 0]
       (do (write-binary-file f small-sample-byte-array)
           (initialize (.getCanonicalPath f))
           @*registers*))
    "Ensure all registers preset with 0, v2"
    (= [0 0 0 0 0 0 0 0]
       (do (write-binary-file f small-sample-byte-array)
           (set-register-value 1 1)
           (initialize (.getCanonicalPath f))
           @*registers*))
    "Ensure program counter set to 0, v1"
    (= 0
       (do (write-binary-file f small-sample-byte-array)
           (initialize (.getCanonicalPath f))
           @*pc*))
    "Ensure program counter set to 0, v2"
    (= 0
       (do (write-binary-file f small-sample-byte-array)
           (reset! *pc* 22)
           (initialize (.getCanonicalPath f))
           @*pc*))))

(spec-report ((find-spec 'bound-variable.core-test)))
