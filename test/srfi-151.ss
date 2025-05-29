(import (scheme base)
        (scheme process-context)
        (srfi 78)
        (srfi 151))

(check (bitwise-not 10) => -11)

(check (bitwise-not -37) => 36)

(check (bitwise-ior  3 10) =>  11)
(check (bitwise-and 11 26) =>  10)
(check (bitwise-xor  3 10) =>   9)
(check (bitwise-eqv 37 12) => -42)
(check (bitwise-and 37 12) =>   4)

(check (bitwise-nand  11 26) => -11)
(check (bitwise-nor   11 26) => -28)
(check (bitwise-andc1 11 26) =>  16)
(check (bitwise-andc2 11 26) =>   1)
(check (bitwise-orc1  11 26) =>  -2)
(check (bitwise-orc2  11 26) => -17)

(check (arithmetic-shift 8  2) => 32)
(check (arithmetic-shift 4  0) =>  4)
(check (arithmetic-shift 8 -1) =>  4)
(check (arithmetic-shift -100000000000000000000000000000000 -100) => -79)

(check (bit-count   0) => 0)
(check (bit-count  -1) => 0)
(check (bit-count   7) => 3)
(check (bit-count  13) => 3)
(check (bit-count -13) => 2)
(check (bit-count  30) => 4)
(check (bit-count -30) => 4)
(check (bit-count (expt 2 100)) =>  1)
(check (bit-count (- (expt 2 100))) => 100)
(check (bit-count (- (+ 1 (expt 2 100)))) =>  1)

(check (integer-length  0) => 0)
(check (integer-length  1) => 1)
(check (integer-length -1) => 0)
(check (integer-length  7) => 3)
(check (integer-length -7) => 3)
(check (integer-length  8) => 4)
(check (integer-length -8) => 3)

(check (bitwise-if 3 1 8) => 9)
(check (bitwise-if 3 8 1) => 0)
(check (bitwise-if 1 1 2) => 3)
(check (bitwise-if #b00111100 #b11110000 #b00001111) => #b00110011)

(check (bit-set? 1 1) => #f)
(check (bit-set? 0 1) => #t)
(check (bit-set? 3 10) => #t)
(check (bit-set? 1000000 -1) => #t)
(check (bit-set? 2 6) => #t)
(check (bit-set? 0 6) => #f)

(check (copy-bit 0 0 #t) => #b1)
(check (copy-bit 2 0 #t) => #b100)
(check (copy-bit 2 #b1111 #f) => #b1011)

(check (bit-swap 0 2 4) => #b1)

(check (any-bit-set? 3 6) => #t)
(check (any-bit-set? 3 12) => #f)
(check (every-bit-set? 4 6) => #t)
(check (every-bit-set? 7 6) => #f)

(check (first-set-bit 1) => 0)
(check (first-set-bit 2) => 1)
(check (first-set-bit 0) => -1)
(check (first-set-bit 40) => 3)
(check (first-set-bit -28) => 2)
(check (first-set-bit (expt  2 99)) => 99)
(check (first-set-bit (expt -2 99)) => 99)


(check (bit-field #b1101101010 0 4) => #b1010)
(check (bit-field #b1101101010 3 9) => #b101101)
(check (bit-field #b1101101010 4 9) => #b10110)
(check (bit-field #b1101101010 4 10) => #b110110)
(check (bit-field 6 0 1) => 0)
(check (bit-field 6 1 3) => 3)
(check (bit-field 6 2 999) => 1)
(check (bit-field #x100000000000000000000000000000000 128 129) => 1)

(check (bit-field-any? #b1001001 1 6) => #t)
(check (bit-field-any? #b1000001 1 6) => #f)

(check (bit-field-every? #b1011110 1 5) => #t)
(check (bit-field-every? #b1011010 1 5) => #f)

(check (bit-field-clear #b101010 1 4) => #b100000)

(check (bit-field-set #b101010 1 4) => #b101110)

(check (bit-field-replace #b101010 #b010 1 4) => #b100100)
(check (bit-field-replace #b110 1 0 1) => #b111)
(check (bit-field-replace #b110 1 1 2) => #b110)

(check (bit-field-replace-same #b1111 #b0000 1 3) => #b1001)

(check-report)

(exit (check-passed? 77))
