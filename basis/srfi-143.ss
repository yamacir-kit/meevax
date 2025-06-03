(define-library (srfi 143)
  (import (only (meevax integer32)
            integer32?
            integer32-width
            integer32-min
            integer32-max
            )
          (scheme base)
          (scheme inexact)
          (srfi 141)
          (srfi 151)
          )

  (export fx-width
          fx-greatest
          fx-least

          fixnum?
          fx=?
          fx<?
          fx>?
          fx<=?
          fx>=?
          fxzero?
          fxpositive?
          fxnegative?
          fxodd?
          fxeven?
          fxmax
          fxmin

          fx+
          fx-
          fxneg
          fx*
          fxquotient
          fxremainder
          fxabs
          fxsquare
          fxsqrt

          fx+/carry
          fx-/carry
          fx*/carry

          fxnot
          fxand
          fxior
          fxxor
          fxarithmetic-shift
          fxarithmetic-shift-left
          fxarithmetic-shift-right
          fxbit-count
          fxlength
          fxif
          fxbit-set?
          fxcopy-bit
          fxfirst-set-bit
          fxbit-field
          fxbit-field-rotate
          fxbit-field-reverse
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

         (define fxnot bitwise-not)
         (define fxand bitwise-and)
         (define fxior bitwise-ior)
         (define fxxor bitwise-xor)

         (define fxarithmetic-shift       arithmetic-shift)
         (define fxarithmetic-shift-left  arithmetic-shift)
         (define fxarithmetic-shift-right arithmetic-shift)

         (define fxbit-count bit-count)

         (define fxlength integer-length)

         (define fxif bitwise-if)

         (define fxbit-set? bit-set?)

         (define fxcopy-bit copy-bit)

         (define fxfirst-set-bit first-set-bit)

         (define fxbit-field         bit-field)
         (define fxbit-field-rotate  bit-field-rotate)
         (define fxbit-field-reverse bit-field-reverse)
         )
  )
