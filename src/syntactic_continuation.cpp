#include <meevax/kernel/basis.hpp>
#include <meevax/kernel/syntactic_continuation.hpp>
#include <meevax/posix/vt102.hpp>

namespace meevax { inline namespace kernel
{
  #define DEFINE_SYNTAX(IDENTIFIER, TRANSFORMER_SPEC)                          \
  define<syntax>(IDENTIFIER, [this](auto&&... xs)                              \
  {                                                                            \
    return TRANSFORMER_SPEC(std::forward<decltype(xs)>(xs)...);                \
  })

  template <>
  void syntactic_continuation::boot(layer<0>)
  {
    DEFINE_SYNTAX("export", exportation);
    DEFINE_SYNTAX("import", importation);
  }

  template <>
  void syntactic_continuation::boot(layer<1>)
  {
    DEFINE_SYNTAX("begin", sequence);
    DEFINE_SYNTAX("call-with-current-continuation", call_cc);
    DEFINE_SYNTAX("define", definition);
    DEFINE_SYNTAX("fork-with-current-syntactic-continuation", fork);
    DEFINE_SYNTAX("if", conditional);
    DEFINE_SYNTAX("lambda", lambda);
    DEFINE_SYNTAX("quote", quotation);
    DEFINE_SYNTAX("reference", reference);
    DEFINE_SYNTAX("set!", assignment);

    // define<syntax>("cons", [this](
    //   auto&& expression,
    //   auto&& syntactic_environment,
    //   auto&& frames,
    //   auto&& continuation,
    //   auto&&)
    // {
    //   return
    //     compile(
    //       cadr(expression),
    //       syntactic_environment,
    //       frames,
    //       compile(
    //         car(expression),
    //         syntactic_environment,
    //         frames,
    //         cons(
    //           make<instruction>(mnemonic::CONS),
    //           continuation)));
    // });
  }

  template <>
  void syntactic_continuation::boot(layer<2>)
  {
    #define DEFINE_PREDICATE(IDENTIFIER, TYPE)                                 \
    define<procedure>(IDENTIFIER, [](let const & xs)                           \
    {                                                                          \
      if (xs.is<null>())                                                       \
      {                                                                        \
        return f;                                                              \
      }                                                                        \
      else                                                                     \
      {                                                                        \
        for (let const & x : xs)                                               \
        {                                                                      \
          if (x.is<null>() or not x.is<TYPE>())                                \
          {                                                                    \
            return f;                                                          \
          }                                                                    \
        }                                                                      \
                                                                               \
        return t;                                                              \
      }                                                                        \
    })

    #define DEFINE_ELEMENTARY_FUNCTION(SYMBOL, FUNCTION)                       \
    define<procedure>(SYMBOL, [&](auto&& xs)                                   \
    {                                                                          \
      if (let const x = car(xs); x.is<null>())                                 \
      {                                                                        \
        return f;                                                              \
      }                                                                        \
      else if (x.is<exact_integer>())                                          \
      {                                                                        \
        if (const floating_point result {                                      \
              FUNCTION(to_inexact(x.as<exact_integer>()))                      \
            }; result.is_integer())                                            \
        {                                                                      \
          return make<exact_integer>(result.to_string());                      \
        }                                                                      \
        else                                                                   \
        {                                                                      \
          return make(result);                                                 \
        }                                                                      \
      }                                                                        \
      else if (x.is<single_float>())                                           \
      {                                                                        \
        return make(floating_point(FUNCTION(x.as<single_float>())));           \
      }                                                                        \
      else if (x.is<double_float>())                                           \
      {                                                                        \
        return make(floating_point(FUNCTION(x.as<double_float>())));           \
      }                                                                        \
      else                                                                     \
      {                                                                        \
        return f;                                                              \
      }                                                                        \
    })

    /* ---- R7RS 6.1. Equivalence predicates -----------------------------------
     *
     * ┌────────────────────┬────────────┬────────────────────────────────────┐
     * │ Symbol             │ Written in │ Note                               │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ eq?                │ C++        │ Compare memory address of object   │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ eqv?               │ C++        │ Comapre value of same type object  │
     * └────────────────────┴────────────┴────────────────────────────────────┘
     *
     * ---------------------------------------------------------------------- */
    define<procedure>("eq?", [](auto&& xs)
    {
      return car(xs) == cadr(xs) ? t : f;
    });

    define<procedure>("eqv?", [](auto&& xs)
    {
      if (let const lhs { car(xs) }, rhs { cadr(xs) }; eq(lhs, rhs))
      {
        return t;
      }
      else if (lhs.is<null>() and rhs.is<null>())
      {
        return t;
      }
      else if (lhs.is<null>() or rhs.is<null>())
      {
        return f;
      }
      else
      {
        return lhs.eqv(rhs) ? t : f;
      }
    });

    /* ---- 6.2.6 Numerical operations -----------------------------------------
     *
     * ┌────────────────────┬────────────┬────────────────────────────────────┐
     * │ Symbol             │ Written in │ Note                               │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ number?            │ Scheme     │                                    │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ complex?           │ Scheme     │                                    │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ real?              │ Scheme     │                                    │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ rational?          │ Scheme     │                                    │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ integer?           │ Scheme     │                                    │
     * └────────────────────┴────────────┴────────────────────────────────────┘
     *
     * ---------------------------------------------------------------------- */

    /* ---- 6.2.6 numerical operations -----------------------------------------
     *
     * ┌────────────────────┬────────────┬────────────────────────────────────┐
     * │ Symbol             │ Written in │ Note                               │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ COMPLEX?           │ C++        │                                    │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ ratio?             │ C++        │                                    │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ single-float?      │ C++        │                                    │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ double-float?      │ C++        │                                    │
     * └────────────────────┴────────────┴────────────────────────────────────┘
     *
     * ---------------------------------------------------------------------- */

    DEFINE_PREDICATE("COMPLEX?", complex);
    DEFINE_PREDICATE("ratio?", ratio);
    DEFINE_PREDICATE("single-float?", floating_point<float>);
    DEFINE_PREDICATE("double-float?", floating_point<double>);

    /* ---- 6.2.6 Numerical operations -----------------------------------------
     *
     * ┌────────────────────┬────────────┬────────────────────────────────────┐
     * │ Symbol             │ Written in │ Note                               │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ exact?             │ Scheme     │                                    │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ inexact?           │ Scheme     │                                    │
     * └────────────────────┴────────────┴────────────────────────────────────┘
     *
     * ┌────────────────────┬────────────┬────────────────────────────────────┐
     * │ Symbol             │ Written in │ Note                               │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ exact-integer?     │ C++        │                                    │
     * └────────────────────┴────────────┴────────────────────────────────────┘
     *
     * ---------------------------------------------------------------------- */

    DEFINE_PREDICATE("exact-integer?", exact_integer);

    /* ---- 6.2.6 Numerical operations -----------------------------------------
     *
     * ┌────────────────────┬────────────┬────────────────────────────────────┐
     * │ Symbol             │ Written in │ Note                               │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ finite?            │ Scheme     │ inexact library procedure          │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ infinite?          │ Scheme     │ inexact library procedure          │
     * └────────────────────┴────────────┴────────────────────────────────────┘
     *
     * ┌────────────────────┬────────────┬────────────────────────────────────┐
     * │ Symbol             │ Written in │ Note                               │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ nan?               │ C++        │                                    │
     * └────────────────────┴────────────┴────────────────────────────────────┘
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("ieee-nan?", [](auto&& xs)
    {
      return std::all_of(std::begin(xs), std::end(xs), is_nan) ? t : f;
    });

    /* ---- 6.2.6 Numerical operations -----------------------------------------
     *
     * ┌────────────────────┬────────────┬────────────────────────────────────┐
     * │ Symbol             │ Written in │ Note                               │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ =                  │ C++        │ Number::operator ==(const object&) │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ <                  │ C++        │ Number::operator < (const object&) │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ >                  │ C++        │ Number::operator > (const object&) │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ <=                 │ C++        │ Number::operator <=(const object&) │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ >=                 │ C++        │ Number::operator >=(const object&) │
     * └────────────────────┴────────────┴────────────────────────────────────┘
     *
     * ---------------------------------------------------------------------- */

    #define DEFINE_TRANSITIVE_COMPARISON(SYMBOL, COMPARE)                      \
    define<procedure>(#SYMBOL, [](auto&& xs) constexpr                         \
    {                                                                          \
      return std::adjacent_find(std::begin(xs), std::end(xs), std::not_fn(COMPARE)) == std::end(xs) ? t : f; \
    })

    DEFINE_TRANSITIVE_COMPARISON(=,  [](auto&& lhs, auto&& rhs) { return lhs.binding() == rhs; });
    DEFINE_TRANSITIVE_COMPARISON(<,  std::less<void>());
    DEFINE_TRANSITIVE_COMPARISON(<=, std::less_equal<void>());
    DEFINE_TRANSITIVE_COMPARISON(>,  std::greater<void>());
    DEFINE_TRANSITIVE_COMPARISON(>=, std::greater_equal<void>());

    /* ---- 6.2.6 numerical operations -----------------------------------------
     *
     * ┌────────────────────┬────────────┬────────────────────────────────────┐
     * │ Symbol             │ Written in │ Note                               │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ zero?              │ Scheme     │                                    │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ positive?          │ Scheme     │                                    │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ negative?          │ Scheme     │                                    │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ odd?               │ Scheme     │                                    │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ even?              │ Scheme     │                                    │
     * └────────────────────┴────────────┴────────────────────────────────────┘
     *
     * ┌────────────────────┬────────────┬────────────────────────────────────┐
     * │ Symbol             │ Written in │ Note                               │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ max                │ Scheme     │ TODO inexact                       │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ min                │ Scheme     │ TODO inexact                       │
     * └────────────────────┴────────────┴────────────────────────────────────┘
     *
     * ┌────────────────────┬────────────┬────────────────────────────────────┐
     * │ Symbol             │ Written in │ Note                               │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ +                  │ C++        │ std::plus<object>                  │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ *                  │ C++        │ std::multiplies<object>            │
     * └────────────────────┴────────────┴────────────────────────────────────┘
     *
     * ---------------------------------------------------------------------- */

    #define BOILERPLATE(SYMBOL, BASIS)                                         \
    define<procedure>(#SYMBOL, [](auto&& xs)                                   \
    {                                                                          \
      return std::accumulate(std::begin(xs), std::end(xs), make<exact_integer>(BASIS), [](auto&& x, auto&& y) { return x SYMBOL y; }); \
    })

    BOILERPLATE(+, 0);
    BOILERPLATE(*, 1);

    #undef BOILERPLATE

    /* ---- 6.2.6 numerical operations -----------------------------------------
     *
     * ┌────────────────────┬────────────┬────────────────────────────────────┐
     * │ Symbol             │ Written in │ Note                               │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ -                  │ C++        │ std::minus<object>                 │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ /                  │ C++        │ std::divides<object>               │
     * └────────────────────┴────────────┴────────────────────────────────────┘
     *
     * ---------------------------------------------------------------------- */

    #define BOILERPLATE(SYMBOL, BASIS)                                         \
    define<procedure>(#SYMBOL, [](auto&& xs)                                   \
    {                                                                          \
      if (length(xs) < 2)                                                      \
      {                                                                        \
        return std::accumulate(std::begin(xs), std::end(xs), make<exact_integer>(BASIS), [](auto&& x, auto&& y) { return x SYMBOL y; }); \
      }                                                                        \
      else                                                                     \
      {                                                                        \
        const auto basis { std::begin(xs) };                                   \
        return std::accumulate(std::next(basis), std::end(xs), *basis, [](auto&& x, auto&& y) { return x SYMBOL y; }); \
      }                                                                        \
    })

    BOILERPLATE(-, 0);
    BOILERPLATE(/, 1);
    BOILERPLATE(%, 1);

    #undef BOILERPLATE

    /* ---- 6.2.6 Numerical operations -----------------------------------------
     *
     * ┌────────────────────┬────────────┬────────────────────────────────────┐
     * │ Symbol             │ Written in │ Note                               │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ abs                │ Scheme     │                                    │
     * └────────────────────┴────────────┴────────────────────────────────────┘
     *
     * ┌────────────────────┬────────────┬────────────────────────────────────┐
     * │ Symbol             │ Written in │ Note                               │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ floor/             │ Scheme     │                                    │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ floor-quotient     │            │                                    │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ floor-remainder    │            │                                    │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ truncate/          │ Scheme     │                                    │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ truncate-quotient  │            │                                    │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ trucate-remainer   │            │                                    │
     * └────────────────────┴────────────┴────────────────────────────────────┘
     *
     * ---------------------------------------------------------------------- */

    /* ---- 6.2.6 Numerical operations -----------------------------------------
     *
     * ┌────────────────────┬────────────┬────────────────────────────────────┐
     * │ Symbol             │ Written in │ Note                               │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ quotient           │ Scheme     │                                    │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ remainder          │ Scheme     │                                    │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ modulo             │ Scheme     │                                    │
     * └────────────────────┴────────────┴────────────────────────────────────┘
     *
     * ┌────────────────────┬────────────┬────────────────────────────────────┐
     * │ Symbol             │ Written in │ Note                               │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ gcd                │ Scheme     │                                    │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ lcm                │ Scheme     │                                    │
     * └────────────────────┴────────────┴────────────────────────────────────┘
     *
     * ┌────────────────────┬────────────┬────────────────────────────────────┐
     * │ Symbol             │ Written in │ Note                               │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ numerator          │ Scheme     │                                    │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ denominator        │ Scheme     │                                    │
     * └────────────────────┴────────────┴────────────────────────────────────┘
     *
     * ┌────────────────────┬────────────┬────────────────────────────────────┐
     * │ Symbol             │ Written in │ Note                               │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ floor              │ C++        │                                    │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ ceiling            │ C++        │                                    │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ truncate           │ C++        │                                    │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ round              │            │                                    │
     * └────────────────────┴────────────┴────────────────────────────────────┘
     *
     * ---------------------------------------------------------------------- */

    DEFINE_ELEMENTARY_FUNCTION("floor", std::floor); // XXX DIRTY HACK!
    DEFINE_ELEMENTARY_FUNCTION("ceiling", std::ceil); // XXX DIRTY HACK!
    DEFINE_ELEMENTARY_FUNCTION("truncate", std::trunc); // XXX DIRTY HACK!

    /* ---- 6.2.6 Numerical operations -----------------------------------------
     *
     * ┌────────────────────┬────────────┬────────────────────────────────────┐
     * │ Symbol             │ Written in │ Note                               │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ rationalize        │            │                                    │
     * └────────────────────┴────────────┴────────────────────────────────────┘
     *
     * ┌────────────────────┬────────────┬────────────────────────────────────┐
     * │ Symbol             │ Written in │ Note                               │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ exp                │ C++        │ inexact library procedure          │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ log                │ C++        │ inexact library procedure          │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ sin                │ C++        │ inexact library procedure          │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ cos                │ C++        │ inexact library procedure          │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ tan                │ C++        │ inexact library procedure          │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ asin               │ C++        │ inexact library procedure          │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ acos               │ C++        │ inexact library procedure          │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ atan               │ C++        │ inexact library procedure          │
     * └────────────────────┴────────────┴────────────────────────────────────┘
     *
     * ---------------------------------------------------------------------- */

    DEFINE_ELEMENTARY_FUNCTION("exp", std::exp); // natural exponential

    DEFINE_ELEMENTARY_FUNCTION("sin", std::sin);
    DEFINE_ELEMENTARY_FUNCTION("cos", std::cos);
    DEFINE_ELEMENTARY_FUNCTION("tan", std::tan);

    DEFINE_ELEMENTARY_FUNCTION("asin", std::asin);
    DEFINE_ELEMENTARY_FUNCTION("acos", std::acos);
    DEFINE_ELEMENTARY_FUNCTION("atan", std::atan);

    DEFINE_ELEMENTARY_FUNCTION("sinh", std::sinh);
    DEFINE_ELEMENTARY_FUNCTION("cosh", std::cosh);
    DEFINE_ELEMENTARY_FUNCTION("tanh", std::tanh);

    DEFINE_ELEMENTARY_FUNCTION("asinh", std::asinh);
    DEFINE_ELEMENTARY_FUNCTION("acosh", std::acosh);
    DEFINE_ELEMENTARY_FUNCTION("atanh", std::atanh);

    // TODO ln
    // TODO atan & atan2

    /* ---- 6.2.6 numerical operations -----------------------------------------
     *
     * ┌────────────────────┬────────────┬────────────────────────────────────┐
     * │ Symbol             │ Written in │ Note                               │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ square             │ Scheme     │                                    │
     * └────────────────────┴────────────┴────────────────────────────────────┘
     *
     * ┌────────────────────┬────────────┬────────────────────────────────────┐
     * │ Symbol             │ Written in │ Note                               │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ square-root        │ C++        │ sqrt                               │
     * └────────────────────┴────────────┴────────────────────────────────────┘
     *
     * ---------------------------------------------------------------------- */

    DEFINE_ELEMENTARY_FUNCTION("square-root", std::sqrt);

    /* ---- 6.2.6 numerical operations -----------------------------------------
     *
     * ┌────────────────────┬────────────┬────────────────────────────────────┐
     * │ Symbol             │ Written in │ Note                               │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ exact-integer-sqrt │            │                                    │
     * └────────────────────┴────────────┴────────────────────────────────────┘
     *
     * ┌────────────────────┬────────────┬────────────────────────────────────┐
     * │ Symbol             │ Written in │ Note                               │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ exponential        │            │ expt                               │
     * └────────────────────┴────────────┴────────────────────────────────────┘
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("exponential", [](auto&& xs)
    {
      if (const floating_point result { std::pow(inexact(car(xs)), inexact(cadr(xs))) }; result.is_integer())
      {
        return make<exact_integer>(result.value);
      }
      else
      {
        return make(result);
      }
    });

    /* ---- 6.2.6 numerical operations -----------------------------------------
     *
     * ┌────────────────────┬────────────┬────────────────────────────────────┐
     * │ Symbol             │ Written in │ Note                               │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ make-rectangular   │ Scheme     │ complex library procedure          │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ make-polar         │ Scheme     │ complex library procedure          │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ real-part          │ Scheme     │ complex library procedure          │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ imag-part          │ Scheme     │ complex library procedure          │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ magnitude          │ Scheme     │ complex library procedure          │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ angle              │ Scheme     │ complex library procedure          │
     * └────────────────────┴────────────┴────────────────────────────────────┘
     *
     * ┌────────────────────┬────────────┬────────────────────────────────────┐
     * │ Symbol             │ Written in │ Note                               │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ inexact            │ C++        │                                    │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ exact              │ C++        │                                    │
     * └────────────────────┴────────────┴────────────────────────────────────┘
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("exact", [](auto&& xs)
    {
      return make(exact(car(xs)));
    });

    define<procedure>("inexact", [](auto&& xs)
    {
      return make(inexact(car(xs)));
    });

    /* ---- 6.2.7 Numerical input and output -----------------------------------
     *
     * ┌────────────────────┬────────────┬────────────────────────────────────┐
     * │ Symbol             │ Written in │ Note                               │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ number->string     │            │                                    │
     * └────────────────────┴────────────┴────────────────────────────────────┘
     *
     * ┌────────────────────┬────────────┬────────────────────────────────────┐
     * │ Symbol             │ Written in │ Note                               │
     * ├────────────────────┼────────────┼────────────────────────────────────┤
     * │ string->number     │            │                                    │
     * └────────────────────┴────────────┴────────────────────────────────────┘
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("number->string", [](auto&& xs)
    {
      return make_string(boost::lexical_cast<std::string>(car(xs)));
    });

    define<procedure>("string->number", [](let const & xs)
    {
      return make_number(car(xs).as<string>());
    });

    /* ==== R7RS 6.3. Booleans =================================================
     *
     *
     * ====================================================================== */

    /* ==== R7RS 6.4. Pairs and lists ==========================================
     *
     *
     * ====================================================================== */
    DEFINE_PREDICATE("pair?", pair);

    define<procedure>("cons", [](auto&& xs)
    {
      return cons(car(xs), cadr(xs));
    });

    define<procedure>("car", [](auto&& xs)
    {
      return caar(xs);
    });

    define<procedure>("cdr", [](auto&& xs)
    {
      return cdar(xs);
    });

    define<procedure>("set-car!", [](auto&& xs)
    {
      return caar(xs) = cadr(xs);
    });

    define<procedure>("set-cdr!", [](auto&& xs)
    {
      return cdar(xs) = cadr(xs);
    });

    /* ==== R7RS 6.5. Symbols ==================================================
     *
     *
     * ====================================================================== */
    DEFINE_PREDICATE("symbol?", symbol);

    define<procedure>("symbol->string", [this](auto&& xs)
    {
      return read('"' + car(xs).template as<std::string>() + '"');
    });

    define<procedure>("string->symbol", [](auto&& xs)
    {
      return make<symbol>(car(xs).template as<string>());
    });

    /* ==== R7RS 6.6. Characters ===============================================
     *
     *
     * ====================================================================== */
    DEFINE_PREDICATE("char?", character);

    define<procedure>("digit-value", [](auto&& xs)
    {
      try
      {
        return make<exact_integer>(car(xs).template as<character>().display());
      }
      catch (std::runtime_error&)
      {
        return f; // XXX
      }
    });

    define<procedure>("char->integer", [](let const& xs)
    {
      if (xs.is<null>())
      {
        throw error(
          cat("Procedure char->integer got ", xs));
      }
      else if (let const& x = car(xs); x.is<character>())
      {
        return make<exact_integer>(x.as<character>().decode());
      }
      else
      {
        throw error(
          cat("Procedure char-integer got ", xs));
      }
    });

    define<procedure>("integer->char", [](let const& xs)
    {
      if (xs.is<null>())
      {
        throw error(
          cat("Procedure integer->char got ", xs));
      }
      else if (let const& x = car(xs); x.is<exact_integer>())
      {
        return make<character>(
          x.as<exact_integer>().value.convert_to<std::uint32_t>());
      }
      else
      {
        throw error(
          cat("Procedure integer->char got ", xs));
      }
    });

    /* ==== R7RS 6.7. Strings ==================================================
     *
     *
     * ====================================================================== */
    DEFINE_PREDICATE("string?", string);

    define<procedure>("char-cons", [](let const& xs)
    {
      return make<string>(car(xs), cadr(xs));
    });

    define<procedure>("number->string", [](auto&& xs)
    {
      if (car(xs).template is<floating_point<double>>())
      {
        return
          make_string(
            boost::lexical_cast<std::string>(
              car(xs).template as<floating_point<double>>()));
      }
      else if (car(xs).template is<exact_integer>())
      {
        return make_string(car(xs).template as<exact_integer>().value.str());
      }
      else
      {
        throw error("no viable operation 'number->string with ", car(xs));
      }
    });

    define<procedure>("string->number", [](auto&& xs)
    {
      return make_number(car(xs).template as<string>());
    });

    /* ==== R7RS 6.8. Vectors ==================================================
     *
     *
     * ====================================================================== */
    DEFINE_PREDICATE("vector?", vector);

    define<procedure>("make-vector", [](auto&& xs)
    {
      auto v { make<vector>() };

      v.as<vector>().resize(
        static_cast<vector::size_type>(
          car(xs).template as<exact_integer>().value));

      return v;
    });

    define<procedure>("vector", [](auto&& xs)
    {
      return make<vector>(in_range, xs);
    });

    define<procedure>("vector-length", [](auto&& xs)
    {
      return
        make<exact_integer>(
          car(xs).template as<vector>().size());
    });

    define<procedure>("vector-ref", [](auto&& xs)
    {
      return
        car(xs).template as<vector>().at(
          static_cast<vector::size_type>(
            cadr(xs).template as<exact_integer>().value));
    });

    define<procedure>("vector-set!", [](auto&& xs)
    {
      return
        car(xs).template as<vector>().at(
          static_cast<vector::size_type>(
            cadr(xs).template as<exact_integer>().value))
        = caddr(xs);
    });

    define<procedure>("vector->list", [](auto&& xs)
    {
      auto result { unit };

      auto& v { car(xs).template as<vector>() };

      std::for_each(std::rbegin(v), std::rend(v), [&](auto&& each) mutable
      {
        return result = cons(each, result);
      });

      return result;
    });

    define<procedure>("list->vector", [](auto&& xs)
    {
      return make<vector>(in_range, std::begin(car(xs)), std::end(car(xs)));
    });

    // define<procedure>("vector->string", [](auto&& xs)
    // {
    //   return unspecified;
    // });

    // define<procedure>("string->vector", [](auto&& xs)
    // {
    //   return unspecified;
    // });

    // define<procedure>("vector-copy", [](auto&& xs)
    // {
    //   return unspecified;
    // });

    // define<procedure>("vector-copy!", [](auto&& xs)
    // {
    //   return unspecified;
    // });

    // define<procedure>("vector-append", [](auto&& xs)
    // {
    //   return unspecified;
    // });

    // define<procedure>("vector-fill!", [](auto&& xs)
    // {
    //   return unspecified;
    // });

    /* ==== R7RS 6.10. Constrol features =======================================
     *
     *
     * ====================================================================== */
    DEFINE_PREDICATE("native-procedure?", procedure);
    DEFINE_PREDICATE("closure?", closure);
    DEFINE_PREDICATE("continuation?", continuation);

    /* ==== R7RS 6.12. Environments and evaluation =============================
     *
     *
     * ====================================================================== */
    define<procedure>("eval", [](auto&& xs)
    {
      return cadr(xs).template as<syntactic_continuation>().evaluate(car(xs));
    });

    /* ==== R7RS 6.13. Input and output ========================================
     *
     *
     * ====================================================================== */
    DEFINE_PREDICATE("input-port?", input_port);
    DEFINE_PREDICATE("output-port?", output_port);

    define<procedure>("open-input-file", [](auto&& xs)
    {
      return make<input_port>(car(xs).template as<string>());
    });

    define<procedure>("open-output-file", [](auto&& xs)
    {
      return make<output_port>(car(xs).template as<string>());
    });

    define<procedure>("close-input-port", [](auto&& xs)
    {
      car(xs).template as<input_port>().close();
      return unspecified;
    });

    define<procedure>("close-output-port", [](auto&& xs)
    {
      car(xs).template as<output_port>().close();
      return unspecified;
    });

    define<procedure>("read", [this](const object& xs)
    {
      return read(xs ? car(xs).as<input_port>() : current_input_port());
    });

    define<procedure>("eof-object?", [](auto&& xs)
    {
      return car(xs).template is<eof>() ? t : f;
    });

    define<procedure>("eof-object", [](auto&&)
    {
      return eof_object;
    });

    define<procedure>("write", [this](auto&& xs)
    {
      write_to(current_output_port(), car(xs));
      return unspecified;
    });

    #define BOILERPLATE(SUFFIX, TYPENAME)                                      \
    define<procedure>("write-" SUFFIX, [this](let const & xs)                  \
    {                                                                          \
      car(xs).as<TYPENAME>().display_to(cdr(xs).is<null>() ? current_output_port() : cadr(xs).as<output_port>()); \
      return unspecified;                                                      \
    })

    BOILERPLATE("char", character);
    BOILERPLATE("string", string);

    #undef BOILERPLATE

    /* ==== R7RS 6.14. System interface ========================================
     *
     * From (scheme load)
     *   load
     *
     * From (scheme file)
     *   file-exists?
     *   delete-file
     *
     * From (scheme process-context)
     *   command-line
     *   exit
     *   emergency-exit
     *   get-environment-variable
     *   get-environment-variables
     *
     * From (sceheme time)
     *   current-second
     *   current-jiffy
     *   jiffies-per-second
     *
     * From (sceheme base)
     *   features
     *
     * ====================================================================== */
    define<procedure>("load", [this](auto&& xs)
    {
      return load(car(xs).template as<const string>());
    });

    define<procedure>("emergency-exit", [](let const & xs)
    {
      if (xs.is<null>() or not car(xs).is<exact_integer>())
      {
        std::exit(boost::exit_success);
      }
      else
      {
        std::exit(car(xs).as<exact_integer>().value.convert_to<int>());
      }

      return unspecified;
    });

    define<procedure>("linker", [](auto&& xs)
    {
      return make<linker>(car(xs).template as<const string>());
    });

    define<procedure>("procedure", [](const object& xs)
    {
      const std::string name { cadr(xs).as<string>() };
      return make<procedure>(name, car(xs).as<linker>().link<procedure::signature>(name));
    });

    define<procedure>("features", [&](auto&&...)                // (scheme base)
    {
      return current_feature;
    });

    /* ==== R4RS APPENDIX: A compatible low-level macro facility ===============
     *
     *
     * ====================================================================== */
    DEFINE_PREDICATE("syntactic-closure?", syntactic_closure);
    DEFINE_PREDICATE("syntactic-continuation?", syntactic_continuation);

    define<procedure>("identifier?", [](auto&& xs)
    {
      return kernel::is_identifier(car(xs)) ? t : f;
    });

    define<procedure>("syntax", [this](auto&& xs)
    {
      return make<syntactic_closure>(xs ? car(xs) : unspecified, syntactic_environment());
    });
  }

  template <>
  void syntactic_continuation::boot(layer<3>)
  {
    auto port { open_input_string(overture.data()) };

    std::size_t counts {0};

    for (let e = read(port); e != eof_object; e = read(port))
    {
      // NOTE: THIS WILL NEVER SHOWN (OVERTURE LAYER BOOTS BEFORE CONFIGURATION)
      write_to(current_debug_port(),
        "\r\x1B[K", header("overture"), counts++, ": ", car(syntactic_environment()));

      current_interaction_port() << std::flush;

      evaluate(e);
    }

    // NOTE: THIS WILL NEVER SHOWN (OVERTURE LAYER BOOTS BEFORE CONFIGURATION)
    write_to(current_debug_port(), "\n\n");
  }

  template <>
  void syntactic_continuation::boot(layer<4>)
  {
    define<procedure>("print", [](auto&& xs)
    {
      for (let const & x : xs)
      {
        if (x.template is<string>())
        {
          std::cout << static_cast<std::string>(x.template as<string>());
        }
        else
        {
          std::cout << x;
        }
      }

      return unspecified; // TODO standard-output-port
    });

    define<procedure>("iec-60559?", [](auto&&)
    {
      return std::numeric_limits<double>::is_iec559 ? t : f;
    });

    define<procedure>("type-of", [](auto&& xs)
    {
      std::cout << car(xs).type().name() << std::endl;
      return unspecified;
    });
  }
}} // namespace meevax::kernel
