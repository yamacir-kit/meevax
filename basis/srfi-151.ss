(define-library (srfi 151)
  (import (only (meevax number)
            bitwise-not
            bitwise-and bitwise-nand
            bitwise-ior bitwise-nior
            bitwise-xor bitwise-nxor
            bitwise-count-trailing-zeros
            bit-shift
            bit-count
            bit-width
            )
          (scheme base)
    )

  (export bitwise-not
          bitwise-and
          bitwise-ior
          bitwise-xor
          bitwise-eqv
          bitwise-nand
          bitwise-nor
          bitwise-andc1
          bitwise-andc2
          bitwise-orc1
          bitwise-orc2

          arithmetic-shift
          bit-count
          integer-length
          bitwise-if

          bit-set?
          copy-bit
          bit-swap
          any-bit-set?
          every-bit-set?
          first-set-bit

          bit-field
          bit-field-any?
          bit-field-every?
          bit-field-clear
          bit-field-set
          bit-field-replace
          bit-field-replace-same
          ; bit-field-rotate
          ; bit-field-reverse

          ; bits->list
          ; list->bits

          ; bits->vector
          ; vector->bits

          ; bits
          ; bitwise-fold
          ; bitwise-for-each
          ; bitwise-unfold

          ; make-bitwise-generator
          )

  (begin (define bitwise-eqv bitwise-nxor)

         (define bitwise-nor bitwise-nior)

         (define (bitwise-andc1 x y)
           (bitwise-and (bitwise-not x) y))

         (define (bitwise-andc2 x y)
           (bitwise-and x (bitwise-not y)))

         (define (bitwise-orc1 x y)
           (bitwise-ior (bitwise-not x) y))

         (define (bitwise-orc2 x y)
           (bitwise-ior x (bitwise-not y)))

         (define arithmetic-shift bit-shift)

         (define integer-length bit-width)

         (define (bitwise-if mask x y)
           (bitwise-ior (bitwise-and mask x)
                        (bitwise-and (bitwise-not mask) y)))

         (define (bit-set? i x)
           (not (zero? (bitwise-and (arithmetic-shift 1 i) x))))

         (define (copy-bit index i on/off)
           (if on/off
               (bitwise-ior i (arithmetic-shift 1 index))
               (bitwise-and i (bitwise-not (arithmetic-shift 1 index)))))

         (define (bit-swap i j x)
           (copy-bit j (copy-bit i x (bit-set? j x)) (bit-set? i x)))

         (define (any-bit-set? x y)
           (not (zero? (bitwise-and x y))))

         (define (every-bit-set? x y)
           (= x (bitwise-and x y)))

         (define first-set-bit bitwise-count-trailing-zeros)

         (define (bit-field i start end)
           (bitwise-and (- (arithmetic-shift 1 (- end start)) 1)
                        (arithmetic-shift i (- start))))

         (define (bit-field-any? i start end)
           (any-bit-set? (- (arithmetic-shift 1 (- end start)) 1)
                         (arithmetic-shift i (- start))))

         (define (bit-field-every? i start end)
           (every-bit-set? (- (arithmetic-shift 1 (- end start)) 1)
                           (arithmetic-shift i (- start))))

         (define (bit-field-clear i start end)
           (bit-field-replace i 0 start end))

         (define (bit-mask start end)
           (arithmetic-shift (bitwise-not (arithmetic-shift -1 (- end start))) start))

         (define (bit-field-set i start end)
           (bitwise-ior i (bit-mask start end)))

         (define (bit-field-replace destination source start end)
           (bit-field-replace-same destination (arithmetic-shift source start) start end))

         (define (bit-field-replace-same destination source start end)
           (bitwise-if (bit-mask start end) source destination))

    )
  )
