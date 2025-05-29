(define-library (srfi 143)
  (import (only (meevax integer32)
            integer32?
            integer32-width
            integer32-min
            integer32-max
            )
          (scheme base)
          (only (srfi 141) balanced/)
          )

  (export
    ; Constants
    fx-width ; Bound to the value w that specifies the implementation-defined range. (R6RS fixnum-width is a procedure that always returns this value.)
    fx-greatest ; Bound to the value 2w-1-1, the largest representable fixnum. (R6RS greatest-fixnum is a procedure that always returns this value.)
    fx-least ; Bound to the value -2w-1, the smallest representable fixnum. (R6RS least-fixnum is a procedure that always returns this value.)

    ; Predicates
    fixnum? ; Returns #t if obj is an exact integer within the fixnum range, and #f otherwise.
    fx=? ; Semantically equivalent to =.
    fx<? ; Semantically equivalent to <.
    fx>? ; Semantically equivalent to >.
    fx<=? ; Semantically equivalent to <=.
    fx>=? ; Semantically equivalent to >=.
    fxzero? ; Semantically equivalent to zero?.
    fxpositive? ; Semantically equivalent to positive?.
    fxnegative? ; Semantically equivalent to negative?.
    fxodd? ; Semantically equivalent to odd?.
    fxeven? ; Semantically equivalent to even?.
    fxmax ; Semantically equivalent to max.
    fxmin ; Semantically equivalent to min.

    ; Basic arithmetic
    fx+ ; Semantically equivalent to +, but accepts exactly two arguments.
    fx- ; Semantically equivalent to -, but accepts exactly two arguments.
    fxneg ; Semantically equivalent to -, but accepts exactly one argument.
    fx* ; Semantically equivalent to *, but accepts exactly two arguments.
    fxquotient ; Semantically equivalent to quotient.
    fxremainder ; Semantically equivalent to remainder.
    fxabs ; Semantically equivalent to abs. In accordance with the fixnum rule, has undefined results when applied to fx-least.
    fxsquare ; Semantically equivalent to square.
    fxsqrt ; Semantically equivalent to exact-integer-sqrt (not sqrt).

    ; Arithmetic with carry
    fx+/carry
    fx-/carry
    fx*/carry

    ; Bitwise operations
    ; TODO fxnot ; Semantically equivalent to bitwise-not.
    ; TODO fxand ; Semantically equivalent to bitwise-and.
    ; TODO fxior ; Semantically equivalent to bitwise-ior.
    ; TODO fxxor ; Semantically equivalent to bitwise-xor.
    ; TODO fxarithmetic-shift ; Semantically equivalent to arithmetic-shift, except that it is an error for the absolute value of count to exceed w-1.
    ; TODO fxarithmetic-shift-left ; The same as fxarithmetic-shift except that a negative value of count is an error. This is provided for additional efficiency.
    ; TODO fxarithmetic-shift-right ; The same as fxarithmetic-shift except that a non-negative value of count specifies the number of bits to shift right, and a negative value is an error. This is provided for additional efficiency.
    ; TODO fxbit-count ; Semantically equivalent to SRFI 151 bit-count.
    ; TODO fxlength ; Semantically equivalent to integer-length.
    ; TODO fxif ; Semantically equivalent to bitwise-if. It can be implemented as (fxior (fxand mask i) (fxand (fxnot mask) j))).
    ; TODO fxbit-set? ; Semantically equivalent to SRFI 151 bit-set?, except that it is an error for index to be larger than or equal to fx-width.
    ; TODO fxcopy-bit ; Semantically equivalent to SRFI 151 copy-bit, except that it is an error for index to be larger than or equal to fx-width.
    ; TODO fxfirst-set-bit ; Semantically equivalent to first-set-bit.
    ; TODO fxbit-field ; Semantically equivalent to bit-field.
    ; TODO fxbit-field-rotate ; Semantically equivalent to SRFI 151 bit-field-rotate.
    ; TODO fxbit-field-reverse ; Semantically equivalent to bit-field-reverse.
    )

  (begin (define fx-width integer32-width)

         (define fx-greatest integer32-max)

         (define fx-least integer32-min)

         (define fixnum? integer32?)

         (define fx=? =)

         (define fx<? <)

         (define fx>? >)

         (define fx<=? <=)

         (define fx>=? >=)

         (define fxzero? zero?)

         (define fxpositive? positive?)

         (define fxnegative? negative?)

         (define fxodd? odd?)

         (define fxeven? even?)

         (define fxmax max)

         (define fxmin min)

         (define fx+ +)

         (define fx- -)

         (define fxneg -)

         (define fx* *)

         (define fxquotient quotient)

         (define fxremainder remainder)

         (define fxabs abs)

         (define fxsquare square)

         (define fxsqrt sqrt)

         (define (fx+/carry i j k)
           (let*-values (((s) (+ i j k))
                         ((q r) (balanced/ s (expt 2 fx-width))))
             (values r q)))

         (define (fx-/carry i j k)
           (let*-values (((d) (- i j k))
                         ((q r) (balanced/ d (expt 2 fx-width))))
             (values r q)))

         (define (fx*/carry i j k)
           (let*-values (((s) (+ (* i j) k))
                         ((q r) (balanced/ s (expt 2 fx-width))))
             (values r q)))

    )
  )
