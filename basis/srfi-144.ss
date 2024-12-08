(define-library (srfi 144)
  (import (only (meevax inexact)
            FP_FAST_FMA
            FP_ILOGB0
            FP_ILOGBNAN
            binary64-epsilon
            binary64-exponent
            binary64-fractional-part
            binary64-integer-log-binary
            binary64-integral-part
            binary64-log-binary
            binary64-max
            binary64-min
            binary64-normalized-fraction
            binary64-sign-bit
            copy-sign
            e
            euler
            gamma
            load-exponent
            next-after
            phi
            pi
            )
          (only (scheme base)
            *
            /
            define
            expt
            if
            inexact
            values
            )
          (only (scheme inexact)
            cos
            log
            sin
            sqrt
            )
          )

  (export fl-e fl-1/e fl-e-2 fl-e-pi/4 fl-log2-e fl-log10-e fl-log-2 fl-1/log-2
          fl-log-3 fl-log-pi fl-log-10 fl-1/log-10 fl-pi fl-1/pi fl-2pi fl-pi/2
          fl-pi/4 fl-pi-squared fl-degree fl-2/pi fl-2/sqrt-pi fl-sqrt-2
          fl-sqrt-3 fl-sqrt-5 fl-sqrt-10 fl-1/sqrt-2 fl-cbrt-2 fl-cbrt-3
          fl-4thrt-2 fl-phi fl-log-phi fl-1/log-phi fl-euler fl-e-euler
          fl-sin-1 fl-cos-1 fl-gamma-1/2 fl-gamma-1/3 fl-gamma-2/3

          fl-greatest fl-least fl-epsilon fl-fast-fl+* fl-integer-exponent-zero
          fl-integer-exponent-nan

          flonum fladjacent flcopysign make-flonum

          flinteger-fraction flexponent flinteger-exponent
          flnormalized-fraction-exponent flsign-bit

          ; flonum? fl=? fl<? fl>? fl<=? fl>=?
          ; flunordered? flinteger? flzero? flpositive? flnegative?
          ; flodd? fleven? flfinite? flinfinite? flnan?
          ; flnormalized? fldenormalized?
          ;
          ; flmax flmin fl+ fl* fl+* fl- fl/ flabs flabsdiff
          ; flposdiff flsgn flnumerator fldenominator
          ; flfloor flceiling flround fltruncate
          ;
          ; flexp flexp2 flexp-1 flsquare flsqrt flcbrt flhypot flexpt fllog
          ; fllog1+ fllog2 fllog10 make-fllog-base
          ;
          ; flsin flcos fltan flasin flacos flatan
          ; flsinh flcosh fltanh flasinh flacosh flatanh
          ;
          ; flquotient flremainder flremquo
          ;
          ; flgamma flloggamma flfirst-bessel flsecond-bessel
          ; flerf flerfc
    )

  (begin (define fl-e e)

         (define fl-1/e (/ 1 e))

         (define fl-e-2 7.38905609893065) ; (define fl-e-2 (expt e 2)) yields 1 ULP error

         (define fl-e-pi/4 (expt e (/ pi 4)))

         (define fl-log2-e (log e 2))

         (define fl-log10-e 0.4342944819032518) ; (define fl-log10-e (log e 10)) yields 1 ULP error

         (define fl-log-2 (log 2))

         (define fl-1/log-2 (/ 1 (log 2)))

         (define fl-log-3 (log 3))

         (define fl-log-pi (log pi))

         (define fl-log-10 (log 10))

         (define fl-1/log-10 0.4342944819032518) ; (define fl-1/log-10 (/ 1 (log 10))) yields 1 ULP error

         (define fl-pi pi)

         (define fl-1/pi (/ 1 pi))

         (define fl-2pi (* 2 pi))

         (define fl-pi/2 (/ pi 2))

         (define fl-pi/4 (/ pi 4))

         (define fl-pi-squared (expt pi 2))

         (define fl-degree (/ pi 180))

         (define fl-2/pi (/ 2 pi))

         (define fl-2/sqrt-pi (/ 2 (sqrt pi)))

         (define fl-sqrt-2 (sqrt 2))

         (define fl-sqrt-3 (sqrt 3))

         (define fl-sqrt-5 (sqrt 5))

         (define fl-sqrt-10 (sqrt 10))

         (define fl-1/sqrt-2 (/ 1 (sqrt 2)))

         (define fl-cbrt-2 (expt 2 (/ 1 3)))

         (define fl-cbrt-3 (expt 3 (/ 1 3)))

         (define fl-4thrt-2 (expt 2 (/ 1 4)))

         (define fl-phi phi)

         (define fl-log-phi (log phi))

         (define fl-1/log-phi 2.07808692123502753) ; (define fl-1/log-phi (/ 1 fl-log-phi)) yields 1 ULP error

         (define fl-euler euler)

         (define fl-e-euler (expt e euler))

         (define fl-sin-1 (sin 1))

         (define fl-cos-1 (cos 1))

         (define fl-gamma-1/2 (gamma (/ 1 2)))

         (define fl-gamma-1/3 2.67893853470774763) ; (define fl-gamma-1/3 (gamma (/ 1 3))) yields 1 ULP error

         (define fl-gamma-2/3 (gamma (/ 2 3)))

         (define fl-greatest binary64-max)

         (define fl-least binary64-min)

         (define fl-epsilon binary64-epsilon)

         (define fl-fast-fl+* FP_FAST_FMA)

         (define fl-integer-exponent-zero FP_ILOGB0)

         (define fl-integer-exponent-nan FP_ILOGBNAN)

         (define flonum inexact)

         (define fladjacent next-after)

         (define flcopysign copy-sign)

         (define make-flonum load-exponent)

         (define (flinteger-fraction x)
           (values (binary64-integral-part x)
                   (binary64-fractional-part x)))

         (define flexponent binary64-log-binary)

         (define flinteger-exponent binary64-integer-log-binary)

         (define (flnormalized-fraction-exponent x)
           (values (binary64-normalized-fraction x)
                   (binary64-exponent x)))

         (define (flsign-bit x)
           (if (binary64-sign-bit x) 1 0))

         )
  )
