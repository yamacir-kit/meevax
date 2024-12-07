(import (only (meevax inexact) binary64?)
        (scheme base)
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

(check (binary64? fl-greatest) => #t)

(check (binary64? fl-least) => #t)

(check (< 0.0 0.0) => #f)

(check (< 0.0 fl-least) => #t)

(check (binary64? fl-epsilon) => #t)

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

(check-report)

(exit (check-passed? 59))
