(import (only (meevax inexact)
          acosh
          asinh
          atanh
          cosh
          sinh
          tanh
          )
        (scheme base)
        (scheme inexact)
        (scheme process-context)
        (srfi 78)
        (srfi 144))

(check fl-e => 2.718281828459045)

(check fl-1/e => 0.3678794411714423215955238)

(check fl-e-2 => 7.38905609893065)

(check fl-e-pi/4 => 2.1932800507380154566)

(check fl-log2-e => 1.4426950408889634073599246810018921374266)

(check fl-log10-e => 0.4342944819032518276511289)

(check fl-log-2 => 0.6931471805599453094172321)

(check fl-1/log-2 => 1.4426950408889634073599246810018921374266)

(check fl-log-3 => 1.0986122886681096913952452)

(check fl-log-pi => 1.144729885849400174143427)

(check fl-log-10 => 2.3025850929940456840179915)

(check fl-1/log-10 => 0.4342944819032518276511289189166050822944)

(check fl-pi => 3.1415926535897932384626433832795028841972)

(check fl-1/pi => 0.3183098861837906715377675267450287240689)

(check fl-2pi => 6.283185307179586476925287)

(check fl-pi/2 => 1.570796326794896619231322)

(check fl-pi/4 => 0.7853981633974483096156608)

(check fl-pi-squared => 9.869604401089358618834491)

(check fl-degree => 0.0174532925199432957692369076848861271344)

(check fl-2/pi => 0.6366197723675814)

(check fl-2/sqrt-pi => 1.1283791670955126)

(check fl-sqrt-2 => 1.4142135623730950488016887242096980785697)

(check fl-sqrt-3 => 1.7320508075688772935274463415058723669428)

(check fl-sqrt-5 => 2.2360679774997896964091736687311762354406)

(check fl-sqrt-10 => 3.1622776601683793319988935444327185337196)

(check fl-1/sqrt-2 => 0.7071067811865475)

(check fl-cbrt-2 => 1.2599210498948731647672106072782283505703)

(check fl-cbrt-3 => 1.4422495703074083823216383107801095883919)

(check fl-4thrt-2 => 1.1892071150027210667174999705604759152930)

(check fl-phi => 1.6180339887498948482045868343656381177203)

(check fl-log-phi => 0.4812118250596034474977589134243684231352)

(check fl-1/log-phi => 2.0780869212350275376013226061177957677422)

(check fl-euler => 0.5772156649015328606065120900824024310422)

(check fl-e-euler => 1.7810724179901979852365041031071795491696)

(check fl-sin-1 => 0.8414709848078965066525023216302989996226)

(check fl-cos-1 => 0.5403023058681397174009366074420766037323)

(check fl-gamma-1/2 => 1.7724538509055160272981674833411451827975)

(check fl-gamma-1/3 => 2.6789385347077476336556929409746776441287)

(check fl-gamma-2/3 => 1.3541179394264004169452880281545137855193)

(check (flonum? fl-greatest) => #t)
(check (flonum? fl-least) => #t)

(check (< 0.0 0.0) => #f)

(check (< 0.0 fl-least) => #t)

(check (flonum? fl-epsilon) => #t)

(check (boolean? fl-fast-fl+*) => #t)

(check (exact-integer? fl-integer-exponent-zero) => #t)
(check (exact-integer? fl-integer-exponent-nan) => #t)

(check (= (flonum 22/7) fl-pi) => #f)
(check (= (flonum 333/106) fl-pi) => #f)
(check (= (flonum 355/113) fl-pi) => #f)
(check (= (flonum 52163/16604) fl-pi) => #f)
(check (= (flonum 103993/33102) fl-pi) => #f)
(check (= (flonum 104348/33215) fl-pi) => #f)
(check (= (flonum 245850922/78256779) fl-pi) => #t)

(check (fladjacent 0.0 1.0) (=> =) fl-least)

(check (< 0.0 (fladjacent 0.0 1.0) fl-epsilon 1.0 (+ 1.0 fl-epsilon) fl-greatest +inf.0) => #t)

(check (flcopysign 0.0 +inf.0) => 0.0)
(check (flcopysign 0.0 -inf.0) => -0.0)

(check (make-flonum 3.0 4) => 48.0)

(call-with-values
  (lambda () (flinteger-fraction 3.14))
  (lambda (integral fractional)
    (check integral (=> =) 3.0)
    (check fractional (=> =) 0.14)))

(check (flexponent 48.0) => 5.0)
(check (flexponent -48.0) => 5.0)

(check (flinteger-exponent 48.0) => 5)
(check (flinteger-exponent -48.0) => 5)

(call-with-values
  (lambda () (flnormalized-fraction-exponent 48.0))
  (lambda (fraction exponent)
    (check fraction => 0.75)
    (check exponent => 6)))

(check (flsign-bit 3.14) => 0)
(check (flsign-bit -3.14) => 1)

(check (flonum? 1.0) => #t)
(check (flonum? 1.0f0) => #f)

(check (procedure? fl=?) => #t)

(check (procedure? fl<?) => #t)

(check (procedure? fl>?) => #t)

(check (procedure? fl<=?) => #t)

(check (procedure? fl>=?) => #t)

(check (flunordered? 1.0 2.0) => #f)
(check (flunordered? 1.0 +nan.0) => #t)

(check (flinteger? 3.14) => #f)
(check (flinteger? 1.0) => #t)

(check (procedure? flzero?) => #t)

(check (procedure? flpositive?) => #t)

(check (procedure? flnegative?) => #t)

(check (procedure? flodd?) => #t)

(check (procedure? fleven?) => #t)

(check (procedure? flfinite?) => #t)

(check (procedure? flinfinite?) => #t)

(check (procedure? flnan?) => #t)

(check (flnormalized? 1.0) => #t)

(check (fldenormalized? (/ fl-least 2)) => #t)

(check (flmax) => -inf.0)
(check (flmax 0.0) => 0.0)
(check (flmax -1.0 0.0 1.0) => 1.0)

(check (flmin) => +inf.0)
(check (flmin 0.0) => 0.0)
(check (flmin -1.0 0.0 1.0) => -1.0)

(check (procedure? fl+) => #t)

(check (procedure? fl*) => #t)

(check (fl+* 2.0 3.0 4.0) => 10.0)

(check (procedure? fl-) => #t)

(check (procedure? fl/) => #t)

(check (flabs -0.0) => +0.0)
(check (flabs -inf.0) => +inf.0)
(check (flabs +inf.0) => +inf.0)

(check (flabsdiff 0.0 1.0) => 1.0)
(check (flabsdiff +inf.0 -inf.0) => +inf.0)
(check (flabsdiff -inf.0 +inf.0) => +inf.0)
(check (flposdiff 3.0 4.0) => 0.0)

(check (flsgn +inf.0) => 1.0)
(check (flsgn -inf.0) => -1.0)
(check (flsgn +0.0) => 1.0)
(check (flsgn -0.0) => -1.0)

(check (numerator 2.25) => 9.0)

(check (numerator -2.25) => -9.0)

(check (denominator 2.25) => 4.0)

(check (denominator -2.25) => 4.0)

(check (procedure? flfloor) => #t)

(check (procedure? flceiling) => #t)

(check (procedure? flround) => #t)

(check (procedure? fltruncate) => #t)

(check (flexp -0.0) => 1.0)
(check (flexp 0.0) => 1.0)
(check (flexp 1.0) => fl-e)

(check (flexp2 -0.0) => 1.0)
(check (flexp2 0.0) => 1.0)
(check (flexp2 fl-log2-e) => fl-e)

(check (fl+ 1.0 (flexp-1 fl-least)) => 1.0)

(check (flsquare -0.0) => 0.0)
(check (flsquare 0.0) => 0.0)
(check (flsquare 1.0) => 1.0)
(check (flsquare 2.0) => 4.0)

(check (flsqrt -0.0) => -0.0)
(check (flsqrt 0.0) => 0.0)
(check (flsqrt 1.0) => 1.0)
(check (flsqrt 2.0) => fl-sqrt-2)
(check (flsqrt 3.0) => fl-sqrt-3)
(check (flsqrt 5.0) => fl-sqrt-5)
(check (flsqrt 10.0) => fl-sqrt-10)

(check (flcbrt 0.0) => 0.0)
(check (flcbrt 1.0) => 1.0)
(check (flcbrt 1.0) => 1.0)
(check (flcbrt 2.0) => fl-cbrt-2)
(check (flcbrt 3.0) => fl-cbrt-3)

(check (flhypot 0.0 0.0) => 0.0)
(check (flhypot 0.0 1.0) => 1.0)
(check (flhypot 0.0 -1.0) => 1.0)
(check (flhypot 1.0 1.0) => fl-sqrt-2)
(check (flhypot 1.0 2.0) => fl-sqrt-5)
(check (flhypot 1.0 3.0) => fl-sqrt-10)

(check (flexpt 0.0 0.0) => 1.0)
(check (flexpt 1.0 0.0) => 1.0)
(check (flexpt 2.0 1.0) => 2.0)
(check (flexpt 2.0 2.0) => 4.0)
(check (flexpt 2.0 3.0) => 8.0)

(check (fllog 0.0) => -inf.0)
(check (fllog 1.0) => 0.0)
(check (fllog fl-phi) => fl-log-phi)
(check (fllog 2.0) => fl-log-2)
(check (fllog 3.0) => fl-log-3)
(check (fllog fl-pi) => fl-log-pi)
(check (fllog 10.0) => fl-log-10)

(check (fllog1+ 0.0) => 0.0)
(check (fllog1+ fl-least) (=> =) 0.0)
(check (fllog1+ 1.0) => fl-log-2)
(check (fllog1+ 2.0) (=> =) fl-log-3)
(check (fllog1+ 9.0) => fl-log-10)

(check (fllog2 0.0) => -inf.0)
(check (fllog2 1.0) => 0.0)
(check (fllog2 2.0) => 1.0)
(check (fllog2 fl-e) => fl-log2-e)
(check (fllog2 +inf.0) => +inf.0)

(check (fllog10 0.0) => -inf.0)
(check (fllog10 1.0) => 0.0)
(check (fllog10 10.0) => 1.0)
(check (fllog10 fl-e) (=> =) fl-log10-e)
(check (fllog10 +inf.0) => +inf.0)

(check ((make-fllog-base 2.0) 0.0) => -inf.0)
(check ((make-fllog-base 2.0) 1.0) => 0.0)
(check ((make-fllog-base 2.0) 2.0) => 1.0)
(check ((make-fllog-base 2.0) fl-e) => fl-log2-e)
(check ((make-fllog-base 2.0) +inf.0) => +inf.0)

(check ((make-fllog-base 10.0) 0.0) => -inf.0)
(check ((make-fllog-base 10.0) 1.0) => 0.0)
(check ((make-fllog-base 10.0) 10.0) => 1.0)
(check ((make-fllog-base 10.0) fl-e) (=> =) fl-log10-e)
(check ((make-fllog-base 10.0) +inf.0) => +inf.0)

(check flsin => sin)

(check flcos => cos)

(check fltan => tan)

(check flasin => asin)

(check flacos => acos)

(check flatan => atan)

(check flsinh => sinh)

(check flcosh => cosh)

(check fltanh => tanh)

(check flasinh => asinh)

(check flacosh => acosh)

(check flatanh => atanh)

(check (flquotient 3.14 0.5) => 6.0)
(check (flquotient -3.14 0.5) => -6.0)
(check (flquotient 3.14 -0.5) => -6.0)
(check (flquotient -3.14 -0.5) => 6.0)

(check (flremainder 3.14 0.5) (=> =) 0.14)
(check (flremainder -3.14 0.5) (=> =) -0.14)
(check (flremainder 3.14 -0.5) (=> =) 0.14)
(check (flremainder -3.14 -0.5) (=> =) -0.14)

(call-with-values (lambda () (flremquo  5.0  2.0 )) (lambda (r q) (check q =>  2) (check r  =>     1.0)))
(call-with-values (lambda () (flremquo  6.0  4.0 )) (lambda (r q) (check q =>  2) (check r  =>    -2.0)))
(call-with-values (lambda () (flremquo  6.3  3.0 )) (lambda (r q) (check q =>  2) (check r (=> =)  0.3)))
(call-with-values (lambda () (flremquo  6.3 -3.0 )) (lambda (r q) (check q => -2) (check r (=> =)  0.3)))
(call-with-values (lambda () (flremquo -6.3  3.0 )) (lambda (r q) (check q => -2) (check r (=> =) -0.3)))
(call-with-values (lambda () (flremquo -6.3 -3.0 )) (lambda (r q) (check q =>  2) (check r (=> =) -0.3)))
(call-with-values (lambda () (flremquo  6.3  3.15)) (lambda (r q) (check q =>  2) (check r  =>     0.0)))
(call-with-values (lambda () (flremquo  6.0  2.0 )) (lambda (r q) (check q =>  3) (check r  =>     0.0)))

(check (flgamma 0.5) => fl-gamma-1/2)
(check (flgamma 2/3) => fl-gamma-2/3)

(call-with-values (lambda () (flloggamma    0.0)) (lambda (value sign) (check value =>             +inf.0) (check sign => 1.0)))
(call-with-values (lambda () (flloggamma    0.5)) (lambda (value sign) (check value => (log fl-gamma-1/2)) (check sign => 1.0)))
(call-with-values (lambda () (flloggamma    1.0)) (lambda (value sign) (check value =>                0.0) (check sign => 1.0)))
(call-with-values (lambda () (flloggamma +inf.0)) (lambda (value sign) (check value =>             +inf.0) (check sign => 1.0)))

(check (cond-expand (__cpp_lib_math_special_functions (flfirst-bessel 0.0 (* 0/3 fl-pi))) (else 1.0                )) => 1.0                )
(check (cond-expand (__cpp_lib_math_special_functions (flfirst-bessel 0.0 (* 1/3 fl-pi))) (else 0.74407197075292975)) => 0.74407197075292975)
(check (cond-expand (__cpp_lib_math_special_functions (flfirst-bessel 0.0 (* 2/3 fl-pi))) (else 0.16979382182100766)) => 0.16979382182100766)
(check (cond-expand (__cpp_lib_math_special_functions (flfirst-bessel 0.5 (* 0/3 fl-pi))) (else 0.0                )) => 0.0                )
(check (cond-expand (__cpp_lib_math_special_functions (flfirst-bessel 0.5 (* 1/3 fl-pi))) (else 0.67523723711782946)) => 0.67523723711782946)
(check (cond-expand (__cpp_lib_math_special_functions (flfirst-bessel 0.5 (* 2/3 fl-pi))) (else 0.47746482927568606)) => 0.47746482927568606)
(check (cond-expand (__cpp_lib_math_special_functions (flfirst-bessel 1.0 (* 0/3 fl-pi))) (else 0.0                )) => 0.0                )
(check (cond-expand (__cpp_lib_math_special_functions (flfirst-bessel 1.0 (* 1/3 fl-pi))) (else 0.45503061147236740)) => 0.45503061147236740)
(check (cond-expand (__cpp_lib_math_special_functions (flfirst-bessel 1.0 (* 2/3 fl-pi))) (else 0.56886896392288921)) => 0.56886896392288921)

(check (cond-expand (__cpp_lib_math_special_functions (flsecond-bessel 0.0 (* 0/3 fl-pi))) (else -inf.0              )) => -inf.0              )
(check (cond-expand (__cpp_lib_math_special_functions (flsecond-bessel 0.0 (* 1/3 fl-pi))) (else  0.12417445819941761)) =>  0.12417445819941761)
(check (cond-expand (__cpp_lib_math_special_functions (flsecond-bessel 0.0 (* 2/3 fl-pi))) (else  0.51799555016845289)) =>  0.51799555016845289)
(check (cond-expand (__cpp_lib_math_special_functions (flsecond-bessel 0.5 (* 0/3 fl-pi))) (else -inf.0              )) => -inf.0              )
(check (cond-expand (__cpp_lib_math_special_functions (flsecond-bessel 0.5 (* 1/3 fl-pi))) (else -0.38984840061683823)) => -0.38984840061683823)
(check (cond-expand (__cpp_lib_math_special_functions (flsecond-bessel 0.5 (* 2/3 fl-pi))) (else  0.27566444771089593)) =>  0.27566444771089593)
(check (cond-expand (__cpp_lib_math_special_functions (flsecond-bessel 1.0 (* 0/3 fl-pi))) (else -inf.0              )) => -inf.0              )
(check (cond-expand (__cpp_lib_math_special_functions (flsecond-bessel 1.0 (* 1/3 fl-pi))) (else -0.74108949656080647)) => -0.74108949656080647)
(check (cond-expand (__cpp_lib_math_special_functions (flsecond-bessel 1.0 (* 2/3 fl-pi))) (else -0.05472495339562021)) => -0.05472495339562021)

(check-report)

(exit (check-passed? 250))
