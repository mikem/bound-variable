(ns bound-variable.core-test
  (:use [bound-variable.core] :reload-all)
  (:use [com.stuartsierra.lazytest :only (deftest defcontext defsuite)])
  (:use [com.stuartsierra.lazytest.report :only (simple-report)]))

;;; helper functions
(defn exec-and-fetch-register [instruction register]
  (execute-instruction instruction)
  (*registers* register))

(deftest test-get-opcode []
  (= 0x0 (get-opcode 0x01234567))
  (= 0x1 (get-opcode 0x12345678))
  (= 0x2 (get-opcode 0x23456789))
  (= 0x3 (get-opcode 0x3456789a))
  (= 0x4 (get-opcode 0x456789ab))
  (= 0x5 (get-opcode 0x56789abc))
  (= 0x6 (get-opcode 0x6789abcd))
  (= 0x7 (get-opcode 0x789abcde))
  (= 0x8 (get-opcode 0x89abcdef))
  (= 0x9 (get-opcode 0x9abcdef0))
  (= 0xa (get-opcode 0xabcdef01))
  (= 0xb (get-opcode 0xbcdef012))
  (= 0xc (get-opcode 0xcdef0123)))

(simple-report (test-get-opcode))

(deftest test-get-register []
  (= 0x2 (get-register :a 0x000002be)) ; % 0000 0010 1011 1110
                                       ;           ^ ^^
  (= 0x7 (get-register :b 0x000002be)) ; % 0000 0010 1011 1110
                                       ;               ^^ ^
  (= 0x6 (get-register :c 0x000002be)) ; % 0000 0010 1011 1110
                                       ;                   ^^^
  (= 0x4 (get-register :a 0x0000031a)) ; % 0000 0011 0001 1010
                                       ;           ^ ^^
  (= 0x3 (get-register :b 0x00000098)) ; % 0000 0000 1001 1000
                                       ;               ^^ ^
  (= 0x7 (get-register :c 0x0000000f)) ; % 0000 0000 0000 1111
                                       ;                   ^^^
  (= 0x4 (get-register :a 0x0000011a)) ; % 0000 0001 0001 1010
                                       ;           ^ ^^
  (= 0x3 (get-register :b 0x00000018)) ; % 0000 0000 0001 1000
                                       ;               ^^ ^
  (= 0x7 (get-register :c 0x00000007)) ; % 0000 0000 0000 0111
                                       ;                   ^^^
  (= 0x0 (get-register :a 0x00000000))
  (= 0x0 (get-register :b 0x00000000))
  (= 0x0 (get-register :c 0x00000000)))

(simple-report (test-get-register))

(defcontext setup-registers []
  (dosync (ref-set *registers* [1 2 3 0 0 4294967296 4294967276 0]))
  :after [_]
  (dosync (ref-set *registers* [0 0 0 0 0 0 0 0])))

(deftest test-get-register-value [_ setup-registers]
  (= 2 (get-register-value :a 0x00000042))  ; % 0000 0000 0100 0010
  (= 1 (get-register-value :b 0x00000042))  ; % 0000 0000 0100 0010
  (= 3 (get-register-value :c 0x00000042))) ; % 0000 0000 0100 0010

(simple-report (test-get-register-value))

(deftest test-exec-operator-0-0 [_ setup-registers]
  "Move contents of B to A because contents of C is non-zero"
  (= 0x3 (exec-and-fetch-register 0x000001d0 7))) ; % 0000 0001 1101 0000

(deftest test-exec-operator-0-1 [_ setup-registers]
  "Do nothing because contents of C is zero"
  (= 0x0 (exec-and-fetch-register 0x000001d4 7))) ; % 0000 0001 1101 0011

(defsuite test-exec-operator-0 []
  test-exec-operator-0-0
  test-exec-operator-0-1)

(simple-report (test-exec-operator-0))

(deftest test-exec-operator-3-0 [_ setup-registers]
  "4 = 3 + 1 (A = B + C)"
  (= 4 (exec-and-fetch-register 0x300000d0 3))) ; % 0000 0000 1101 0000

(deftest test-exec-operator-3-1 [_ setup-registers]
  "4 = 2 + 2 (A = B + C)"
  (= 4 (exec-and-fetch-register 0x30000109 4))) ; % 0000 0001 0000 1001

(deftest test-exec-operator-3-2 [_ setup-registers]
  "5 = 2 + 3 (A = B + C)"
  (= 5 (exec-and-fetch-register 0x3000014a 5))) ; % 0000 0001 0100 1010

(deftest test-exec-operator-3-3 [_ setup-registers]
  "1 = 4294967296 + 1 (A = B + C)"
  (= 1 (exec-and-fetch-register 0x300001a8 6))) ; % 0000 0001 1010 1000

(defsuite test-exec-operator-3 []
  test-exec-operator-3-0
  test-exec-operator-3-1
  test-exec-operator-3-2
  test-exec-operator-3-3)

(simple-report (test-exec-operator-3))

  ;(dosync (ref-set *registers* [1 2 3 0 0 4294967296 4294967276 0]))
(deftest test-exec-operator-4-0 [_ setup-registers]
  "6 = 3 * 2 (A = B * C)"
  (= 6 (exec-and-fetch-register 0x400001d1 7))) ; % 0000 0001 1101 0001

(deftest test-exec-operator-4-1 [_ setup-registers]
  "9 = 3 * 3 (A = B * C)"
  (= 9 (exec-and-fetch-register 0x40000092 2))) ; % 0000 0000 1001 0010

(deftest test-exec-operator-4-2 [_ setup-registers]
  "4294967256 = 4294967276 * 2 (A = B + C)"
  (= 4294967256 (exec-and-fetch-register 0x400000f1 3))) ; % 0000 0000 1111 0001

(defsuite test-exec-operator-4 []
  test-exec-operator-4-0
  test-exec-operator-4-1
  test-exec-operator-4-2)

(simple-report (test-exec-operator-4))
