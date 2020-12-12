#include <iterator>
#include <meevax/kernel/basis.hpp>
#include <meevax/kernel/syntactic_continuation.hpp>
#include <meevax/posix/vt102.hpp>

namespace meevax
{
inline namespace kernel
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

  /* ---- R7RS 6.1. Equivalence predicates -------------------------------------

      ┌────────────────────┬────────────┬────────────────────────────────────┐
      │ Symbol             │ Written in │ Note                               │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ eq?                │ C++        │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ eqv?               │ C++        │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ equal?             │ Scheme     │                                    │
      └────────────────────┴────────────┴────────────────────────────────────┘

     ------------------------------------------------------------------------ */

    define<procedure>("eq?", [](auto&& xs)
    {
      return car(xs) == cadr(xs) ? t : f;
    });

    define<procedure>("eqv?", [](auto&& xs)
    {
      if (let const lhs = car(xs), rhs = cadr(xs); eq(lhs, rhs))
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


  /* ---- R7RS 6.2. Numbers ----------------------------------------------------

      Non-standard procedures
     -------------------------
     ┌────────────────────┬────────────┬───────────────────────────────────────┐
     │ Symbol             │ Written in │ Note                                  │
     ├────────────────────┼────────────┼───────────────────────────────────────┤
     │ COMPLEX?           │ C++        │                                       │
     ├────────────────────┼────────────┼───────────────────────────────────────┤
     │ ratio?             │ C++        │                                       │
     ├────────────────────┼────────────┼───────────────────────────────────────┤
     │ single-float?      │ C++        │                                       │
     ├────────────────────┼────────────┼───────────────────────────────────────┤
     │ double-float?      │ C++        │                                       │
     └────────────────────┴────────────┴───────────────────────────────────────┘

      6.2.6. Numerical operations
     -----------------------------

     ┌────────────────────┬────────────┬────────────────────────────────────┐
     │ Symbol             │ Written in │ Note                               │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ number?            │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ complex?           │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ real?              │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ rational?          │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ integer?           │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ exact?             │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ inexact?           │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ exact-integer?     │ C++        │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ finite?            │ Scheme     │ inexact library procedure          │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ infinite?          │ Scheme     │ inexact library procedure          │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ nan?               │ C++        │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ =                  │ C++        │ Number::operator ==(const object&) │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ <                  │ C++        │ Number::operator < (const object&) │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ >                  │ C++        │ Number::operator > (const object&) │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ <=                 │ C++        │ Number::operator <=(const object&) │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ >=                 │ C++        │ Number::operator >=(const object&) │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ zero?              │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ positive?          │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ negative?          │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ odd?               │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ even?              │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ max                │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ min                │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ +                  │ C++        │ std::plus<object>                  │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ *                  │ C++        │ std::multiplies<object>            │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ -                  │ C++        │ std::minus<object>                 │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ /                  │ C++        │ std::divides<object>               │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ abs                │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ floor/             │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ floor-quotient     │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ floor-remainder    │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ truncate/          │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ truncate-quotient  │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ trucate-remainer   │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ quotient           │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ remainder          │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ modulo             │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ gcd                │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ lcm                │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ numerator          │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ denominator        │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ floor              │ C++        │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ ceiling            │ C++        │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ truncate           │ C++        │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ round              │ TODO       │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ rationalize        │ TODO       │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ exp                │ C++        │ inexact library procedure          │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ log                │ C++        │ inexact library procedure          │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ sin                │ C++        │ inexact library procedure          │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ cos                │ C++        │ inexact library procedure          │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ tan                │ C++        │ inexact library procedure          │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ asin               │ C++        │ inexact library procedure          │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ acos               │ C++        │ inexact library procedure          │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ atan               │ C++        │ inexact library procedure          │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ square             │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ square-root        │ C++        │ sqrt                               │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ exact-integer-sqrt │ TODO       │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ exponential        │ TODO       │ expt                               │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ make-rectangular   │ Scheme     │ complex library procedure          │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ make-polar         │ Scheme     │ complex library procedure          │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ real-part          │ Scheme     │ complex library procedure          │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ imag-part          │ Scheme     │ complex library procedure          │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ magnitude          │ Scheme     │ complex library procedure          │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ angle              │ Scheme     │ complex library procedure          │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ inexact            │ C++        │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ exact              │ C++        │                                    │
     └────────────────────┴────────────┴────────────────────────────────────┘

      6.2.7. Numerical input and output
    ------------------------------------
     ┌────────────────────┬────────────┬────────────────────────────────────┐
     │ Symbol             │ Written in │ Note                               │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ number->string     │ TODO       │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ string->number     │ TODO       │                                    │
     └────────────────────┴────────────┴────────────────────────────────────┘

     ------------------------------------------------------------------------ */

    define<procedure>("COMPLEX?", make_predicate<complex>());

    define<procedure>("ratio?", make_predicate<ratio>());

    define<procedure>("single-float?", make_predicate<single_float>());
    define<procedure>("double-float?", make_predicate<double_float>());


    define<procedure>("exact-integer?", make_predicate<exact_integer>());


    define<procedure>("ieee-nan?", [](auto&& xs)
    {
      return std::all_of(std::begin(xs), std::end(xs), is_nan) ? t : f;
    });


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


    #define BOILERPLATE(SYMBOL, BASIS)                                         \
    define<procedure>(#SYMBOL, [](auto&& xs)                                   \
    {                                                                          \
      return std::accumulate(std::begin(xs), std::end(xs), make<exact_integer>(BASIS), [](auto&& x, auto&& y) { return x SYMBOL y; }); \
    })

    BOILERPLATE(+, 0);
    BOILERPLATE(*, 1);

    #undef BOILERPLATE


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


    DEFINE_ELEMENTARY_FUNCTION("floor", std::floor); // XXX DIRTY HACK!
    DEFINE_ELEMENTARY_FUNCTION("ceiling", std::ceil); // XXX DIRTY HACK!
    DEFINE_ELEMENTARY_FUNCTION("truncate", std::trunc); // XXX DIRTY HACK!


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


    DEFINE_ELEMENTARY_FUNCTION("square-root", std::sqrt);


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


    define<procedure>("exact", [](auto&& xs)
    {
      return make(exact(car(xs)));
    });

    define<procedure>("inexact", [](auto&& xs)
    {
      return make(inexact(car(xs)));
    });


    define<procedure>("number->string", [](auto&& xs)
    {
      return make_string(boost::lexical_cast<std::string>(car(xs)));
    });

    define<procedure>("string->number", [](let const & xs)
    {
      return make_number(car(xs).as<string>());
    });


  /* ---- R7RS 6.3. Booleans ---------------------------------------------------

      ┌────────────────────┬────────────┬────────────────────────────────────┐
      │ Symbol             │ Written in │ Note                               │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ not                │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ boolean?           │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ boolean=?          │ Scheme     │                                    │
      └────────────────────┴────────────┴────────────────────────────────────┘

     ------------------------------------------------------------------------ */


  /* ---- R7RS 6.4. Pairs and lists --------------------------------------------

      ┌────────────────────┬────────────┬────────────────────────────────────┐
      │ Symbol             │ Written in │ Note                               │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ pair?              │ C++        │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ cons               │ C++        │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ car                │ C++        │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ cdr                │ C++        │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ set-car!           │ C++        │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ set-cdr!           │ C++        │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ caar               │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ cadr               │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ cdar               │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ cddr               │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ caaar ... cddddr   │ Scheme     │ (scheme cxr) library               │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ null?              │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ list?              │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ make-list          │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ list               │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ length             │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ append             │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ reverse            │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ list-tail          │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ list-ref           │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ list-set!          │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ memq               │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ memv               │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ member             │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ assq               │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ assv               │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ assoc              │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ list-copy          │ Scheme     │                                    │
      └────────────────────┴────────────┴────────────────────────────────────┘

    ------------------------------------------------------------------------- */

    define<procedure>("pair?", make_predicate<pair>());

    define<procedure>("cons", [](auto&& xs)
    {
      return cons(car(xs), cadr(xs));
    });

    define<procedure>("car", [](auto&& xs) { return caar(xs); });
    define<procedure>("cdr", [](auto&& xs) { return cdar(xs); });

    define<procedure>("set-car!", [](auto&& xs) { return caar(xs) = cadr(xs); });
    define<procedure>("set-cdr!", [](auto&& xs) { return cdar(xs) = cadr(xs); });


  /* ---- R7RS 6.5. Symbols ----------------------------------------------------

      ┌────────────────────┬────────────┬────────────────────────────────────┐
      │ Symbol             │ Written in │ Note                               │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ symbol?            │ C++        │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ symbol=?           │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ symbol->string     │ C++        │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string->symbol     │ C++        │                                    │
      └────────────────────┴────────────┴────────────────────────────────────┘

    ------------------------------------------------------------------------- */

    define<procedure>("symbol?", make_predicate<symbol>());

    define<procedure>("symbol->string", [](let const& xs)
    {
      return make_string(car(xs).as<symbol>());
    });

    define<procedure>("string->symbol", [](let const& xs)
    {
      return make<symbol>(car(xs).as<string>());
    });


  /* ---- R7RS 6.6. Characters -------------------------------------------------

      ┌────────────────────┬────────────┬────────────────────────────────────┐
      │ Symbol             │ Written in │ Note                               │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ char?              │ C++        │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ char=?             │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ char<?             │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ char>?             │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ char<=?            │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ char>=?            │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ char-ci=?          │ Scheme     │ (scheme char) library              │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ char-ci<?          │ Scheme     │ (scheme char) library              │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ char-ci>?          │ Scheme     │ (scheme char) library              │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ char-ci<=?         │ Scheme     │ (scheme char) library              │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ char-ci>=?         │ Scheme     │ (scheme char) library              │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ char-alphabetic?   │ Scheme     │ (scheme char) library              │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ char-numeric?      │ Scheme     │ (scheme char) library              │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ char-whitespace?   │ Scheme     │ (scheme char) library              │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ char-upper-case?   │ Scheme     │ (scheme char) library              │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ char-lower-case?   │ Scheme     │ (scheme char) library              │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ digit-value        │ C++        │ (scheme char) library              │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ char->integer      │ C++        │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ integer->char      │ C++        │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ char-upcase        │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ char-downcase      │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ char-foldcase      │ Scheme     │                                    │
      └────────────────────┴────────────┴────────────────────────────────────┘

    ------------------------------------------------------------------------- */

    define<procedure>("char?", make_predicate<character>());

    define<procedure>("digit-value", [](let const& xs)
    {
      try
      {
        return make<exact_integer>(car(xs).as<character>().write_char());
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
        return make<exact_integer>(x.as<character>().codepoint());
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


  /* ---- R7RS 6.7. Strings ----------------------------------------------------

      ┌────────────────────┬────────────┬────────────────────────────────────┐
      │ Symbol             │ Written in │ Note                               │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string?            │ C++        │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ make-string        │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string             │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string-length      │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string-ref         │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string-set!        │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string=?           │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string<?           │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string>?           │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string<=?          │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string>=?          │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string-ci=?        │ Scheme     │ (scheme char) library              │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string-ci<?        │ Scheme     │ (scheme char) library              │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string-ci>?        │ Scheme     │ (scheme char) library              │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string-ci<=?       │ Scheme     │ (scheme char) library              │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string-ci>=?       │ Scheme     │ (scheme char) library              │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string-upcase      │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string-downcase    │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string-foldcase    │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ substring          │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string-append      │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string->list       │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ list->string       │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string-copy        │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string-copy!       │ TODO       │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string-fill!       │ Scheme     │                                    │
      └────────────────────┴────────────┴────────────────────────────────────┘

    ------------------------------------------------------------------------- */

    define<procedure>("string?", make_predicate<string>());

    define<procedure>("char-cons", [](let const& xs)
    {
      return make<string>(car(xs), cadr(xs));
    });

    define<procedure>("number->string", [](let const& xs)
    {
      if (car(xs).is<double_float>())
      {
        return
          make_string(
            boost::lexical_cast<std::string>(
              car(xs).as<double_float>()));
      }
      else if (car(xs).is<exact_integer>())
      {
        return make_string(car(xs).as<exact_integer>().value.str());
      }
      else
      {
        throw error("no viable operation 'number->string with ", car(xs));
      }
    });

    define<procedure>("string->number", [](let const& xs)
    {
      return make_number(car(xs).as<string>());
    });


  /* ---- R7RS 6.8. Vectors ----------------------------------------------------

     ┌────────────────────┬────────────┬────────────────────────────────────┐
     │ Symbol             │ Written in │ Note                               │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ vector?            │ C++        │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ make-vector?       │ C++        │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ vector             │ C++        │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ vector-length      │ C++        │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ vector-ref         │ C++        │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ vector-set!        │ C++        │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ vector->list       │ C++        │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ list->vector       │ C++        │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ vector->string     │ TODO       │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ string->vector     │ TODO       │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ vector-copy        │ TODO       │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ vector-copy!       │ TODO       │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ vector-append      │ TODO       │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ vector-fill!       │ TODO       │                                    │
     └────────────────────┴────────────┴────────────────────────────────────┘

    ------------------------------------------------------------------------- */

    define<procedure>("vector?", make_predicate<vector>());

    define<procedure>("make-vector", [](let const& xs)
    {
      let v = make<vector>();

      v.as<vector>().resize(
        static_cast<vector::size_type>(
          car(xs).as<exact_integer>().value));

      return v;
    });

    define<procedure>("vector", [](auto&& xs)
    {
      return make<vector>(in_range, std::forward<decltype(xs)>(xs));
    });

    define<procedure>("vector-length", [](let const& xs)
    {
      return make<exact_integer>(car(xs).as<vector>().size());
    });

    define<procedure>("vector-ref", [](let const& xs)
    {
      return
        car(xs).as<vector>().at(
          static_cast<vector::size_type>(
            cadr(xs).as<exact_integer>().value));
    });

    define<procedure>("vector-set!", [](let const& xs)
    {
      return
        car(xs).as<vector>().at(
          static_cast<vector::size_type>(
            cadr(xs).as<exact_integer>().value))
        = caddr(xs);
    });

    define<procedure>("vector->list", [](let const& xs)
    {
      let result = unit;

      auto& v { car(xs).as<vector>() };

      std::for_each(std::rbegin(v), std::rend(v), [&](auto&& each)
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


  /* ---- R7RS 6.10. Control features ------------------------------------------

      Non-standard procedures
      -----------------------

     ┌────────────────────┬────────────┬────────────────────────────────────┐
     │ Symbol             │ Written in │ Note                               │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ native-procedure?  │ C++        │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ closure?           │ C++        │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ continuation?      │ C++        │                                    │
     └────────────────────┴────────────┴────────────────────────────────────┘

      Standard procedures
      -------------------

     ┌────────────────────┬────────────┬────────────────────────────────────┐
     │ Symbol             │ Written in │ Note                               │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ procedure?         │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ apply              │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ map                │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ string-map         │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ vector-map         │ TODO       │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ for-each           │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ string-for-each    │ TODO       │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ vector-for-each    │ TODO       │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ call/cc            │ C++/Scheme │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ values             │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ call-with-values   │ Scheme     │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ dynamic-wind       │ Scheme     │                                    │
     └────────────────────┴────────────┴────────────────────────────────────┘

    ------------------------------------------------------------------------- */

    define<procedure>("native-procedure?", make_predicate<procedure>());

    define<procedure>("closure?", make_predicate<closure>());

    define<procedure>("continuation?", make_predicate<continuation>());


  /* ---- R7RS 6.11. Exceptions ------------------------------------------------

      Standard procedures
      -------------------

     ┌────────────────────────┬────────────┬──────────────────────────────────┐
     │ Symbol                 │ Written in │ Note                             │
     ├────────────────────────┼────────────┼──────────────────────────────────┤
     │ with-exception-handler │ TODO       │                                  │
     ├────────────────────────┼────────────┼──────────────────────────────────┤
     │ raise                  │ TODO       │                                  │
     ├────────────────────────┼────────────┼──────────────────────────────────┤
     │ raise-continuable      │ TODO       │                                  │
     ├────────────────────────┼────────────┼──────────────────────────────────┤
     │ error                  │ TODO       │                                  │
     ├────────────────────────┼────────────┼──────────────────────────────────┤
     │ error-object?          │ TODO       │                                  │
     ├────────────────────────┼────────────┼──────────────────────────────────┤
     │ error-object-message   │ TODO       │                                  │
     ├────────────────────────┼────────────┼──────────────────────────────────┤
     │ error-object-irritants │ TODO       │                                  │
     ├────────────────────────┼────────────┼──────────────────────────────────┤
     │ read-error?            │ TODO       │                                  │
     ├────────────────────────┼────────────┼──────────────────────────────────┤
     │ file-error?            │ TODO       │                                  │
     └────────────────────────┴────────────┴──────────────────────────────────┘

    ------------------------------------------------------------------------- */


  /* ---- R7RS 6.12. Environments and evaluation -------------------------------

      Standard procedures
      -------------------

     ┌───────────────────────────┬────────────┬───────────────────────────────┐
     │ Symbol                    │ Written in │ Note                          │
     ├───────────────────────────┼────────────┼───────────────────────────────┤
     │ environment               │ TODO       │ (scheme eval) library         │
     ├───────────────────────────┼────────────┼───────────────────────────────┤
     │ scheme-report-environment │ TODO       │ (scheme r5rs) library         │
     ├───────────────────────────┼────────────┼───────────────────────────────┤
     │ null-environment          │ TODO       │ (scheme r5rs) library         │
     ├───────────────────────────┼────────────┼───────────────────────────────┤
     │ interaction-environment   │ TODO       │ (scheme repl) library         │
     ├───────────────────────────┼────────────┼───────────────────────────────┤
     │ eval                      │ TODO       │ (scheme eval) library         │
     └───────────────────────────┴────────────┴───────────────────────────────┘

    ------------------------------------------------------------------------- */

    define<procedure>("eval", [](let const& xs)
    {
      return cadr(xs).as<syntactic_continuation>().evaluate(car(xs));
    });


  /* ---- R7RS 6.13. Input and output ------------------------------------------

       Non-standard procedures
       -----------------------
      ┌─────────────────────────┬────────────┬───────────────────────────────┐
      │ Identifier              │ Written in │ Note                          │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ standard-input-port     │ C++        │ std::cin                      │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ standard-output-port    │ C++        │ std::cout                     │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ standard-error-port     │ C++        │ std::cerr                     │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ input-file-port?        │ C++        │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ output-file-port?       │ C++        │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ input-string-port?      │ C++        │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ output-string-port?     │ C++        │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ input-file-port-open?   │ C++        │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ output-file-port-open?  │ C++        │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ close-input-file-port   │ C++        │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ close-output-file-port  │ C++        │                               │
      └─────────────────────────┴────────────┴───────────────────────────────┘


       6.13.1. Port
       ------------
      ┌─────────────────────────┬────────────┬───────────────────────────────┐
      │ Identifier              │ Written in │ Note                          │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ call-with-port          │ Scheme     │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ call-with-input-file    │ Scheme     │ (scheme file) library         │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ call-with-output-file   │ Scheme     │ (scheme file) library         │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ input-port?             │ Scheme     │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ output-port?            │ Scheme     │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ textual-port?           │ Scheme     │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ binary-port?            │ TODO       │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ port?                   │ Scheme     │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ input-port-open?        │ Scheme     │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ output-port-open?       │ Scheme     │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ current-input-port      │ Scheme     │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ current-output-port     │ Scheme     │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ current-error-port      │ Scheme     │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ with-input-from-file    │ Scheme     │ (scheme file) library         │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ with-output-to-file     │ Scheme     │ (scheme file) library         │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ open-input-file         │ C++        │ (scheme file) library         │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ open-binary-input-file  │ TODO       │ (scheme file) library         │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ open-output-file        │ C++        │ (scheme file) library         │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ open-binary-output-file │ TODO       │ (scheme file) library         │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ close-port              │ Scheme     │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ close-input-port        │ Scheme     │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ close-output-port       │ Scheme     │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ open-input-string       │ C++        │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ open-output-string      │ C++        │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ get-output-string       │ C++        │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ open-input-bytevector   │ TODO       │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ open-output-bytevector  │ TODO       │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ get-output-bytevector   │ TODO       │                               │
      └─────────────────────────┴────────────┴───────────────────────────────┘


       6.13.2. Input
       -------------
      ┌─────────────────────────┬────────────┬───────────────────────────────┐
      │ Symbol                  │ Written in │ Note                          │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ read                    │ C++/Scheme │ (scheme read) library         │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ read-char               │ C++/Scheme │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ peek-char               │ C++/Scheme │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ read-line               │ TODO       │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ eof-object?             │ C++        │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ eof-object              │ C++        │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ char-ready?             │ C++/Scheme │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ read-string             │ TODO       │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ read-u8                 │ TODO       │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ peek-u8                 │ TODO       │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ u8-ready?               │ TODO       │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ read-bytevector         │ TODO       │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ read-bytevector!        │ TODO       │                               │
      └─────────────────────────┴────────────┴───────────────────────────────┘


       6.13.3. Output
       --------------
      ┌─────────────────────────┬────────────┬───────────────────────────────┐
      │ Symbol                  │ Written in │ Note                          │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ write                   │ TODO       │ (scheme write) library        │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ write-shared            │ TODO       │ (scheme write) library        │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ write-simple            │ Scheme     │ (scheme write) library        │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ display                 │ Scheme     │ (scheme write) library        │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ newline                 │ Scheme     │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ write-char              │ Scheme     │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ write-string            │ Scheme     │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ write-u8                │ TODO       │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ write-bytevector        │ TODO       │                               │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ flush-output-port       │ Scheme     │                               │
      └─────────────────────────┴────────────┴───────────────────────────────┘

    ------------------------------------------------------------------------- */

    define<procedure>("standard-input-port", [this](auto&&)
    {
      return standard_input_port();
    });

    define<procedure>("standard-output-port", [this](auto&&)
    {
      return standard_output_port();
    });

    define<procedure>("standard-error-port", [this](auto&&)
    {
      return standard_error_port();
    });


    define<procedure>("input-file-port?", make_predicate<input_file_port>());

    define<procedure>("output-file-port?", make_predicate<output_file_port>());

    define<procedure>("input-string-port?", make_predicate<input_string_port>());

    define<procedure>("output-string-port?", make_predicate<output_string_port>());


    define<procedure>("input-file-port-open?", [](let const& xs)
    {
      return car(xs).as<input_file_port>().is_open() ? t : f;
    });

    define<procedure>("output-file-port-open?", [](let const& xs)
    {
      return car(xs).as<output_file_port>().is_open() ? t : f;
    });


    define<procedure>("open-input-file", [](let const& xs)
    {
      return make<input_file_port>(car(xs).as<string>());
    });

    define<procedure>("open-output-file", [](let const& xs)
    {
      return make<output_file_port>(car(xs).as<string>());
    });


    define<procedure>("close-input-file-port", [](let const& xs)
    {
      car(xs).as<input_file_port>().close();
      return unspecified;
    });

    define<procedure>("close-output-file-port", [](let const& xs)
    {
      car(xs).as<output_file_port>().close();
      return unspecified;
    });


    define<procedure>("open-input-string", [](let const& xs)
    {
      if (xs.is<null>())
      {
        return make<input_string_port>();
      }
      else if (let const x = car(xs); x.is<string>())
      {
        return make<input_string_port>(x.as<string>());
      }
      else
      {
        throw error("open-input-string: not string", car(xs));
      }
    });

    define<procedure>("open-output-string", [](let const& xs)
    {
      if (xs.is<null>())
      {
        return make<output_string_port>();
      }
      else if (let const x = car(xs); x.is<string>())
      {
        return make<output_string_port>(x.as<string>());
      }
      else
      {
        throw error("open-output-string: not string", car(xs));
      }
    });

    define<procedure>("get-output-string", [](let const& xs)
    {
      return make_string(car(xs).as<output_string_port>().str());
    });


    define<procedure>("::read", [this](let const& xs)
    {
      return read(car(xs));
    });

    define<procedure>("::read-char", [](let const& xs)
    {
      try
      {
        return make<character>(car(xs).as<input_port>());
      }
      catch (read_error<eof> const&)
      {
        return eof_object;
      }
    });

    define<procedure>("::peek-char", [](let const& xs)
    {
      try
      {
        return make<character>(peek_codeunit(car(xs).as<input_port>()));
      }
      catch (read_error<eof> const&)
      {
        return eof_object;
      }
    });


    define<procedure>("eof-object?", make_predicate<eof>());

    define<procedure>("eof-object", [](auto&&)
    {
      return eof_object;
    });


    define<procedure>("::char-ready?", [](let const& xs)
    {
      return car(xs).as<input_port const>() ? t : f;
    });


    define<procedure>("::write-simple", [this](let const& xs)
    {
      write_to(cadr(xs), car(xs));
      return unspecified;
    });

    define<procedure>("::write-char", [](let const& xs)
    {
      car(xs).as<character>().write_char(cadr(xs));
      return unspecified;
    });

    define<procedure>("::write-string", [](let const& xs)
    {
      car(xs).as<string>().write_string(cadr(xs));
      return unspecified;
    });


    define<procedure>("::flush-output-port", [](let const& xs)
    {
      car(xs).as<output_port>() << std::flush;
      return unspecified;
    });


  /* ---- R7RS 6.14. System interface ------------------------------------------

      Standard procedures
      -------------------

     ┌───────────────────────────┬────────────┬──────────────────────────────────┐
     │ Symbol                    │ Written in │ Note                             │
     ├───────────────────────────┼────────────┼──────────────────────────────────┤
     │ load                      │ C++        │ (scheme load) library            │
     ├───────────────────────────┼────────────┼──────────────────────────────────┤
     │ file-exists?              │ TODO       │ (scheme file) library            │
     ├───────────────────────────┼────────────┼──────────────────────────────────┤
     │ delete-file               │ TODO       │ (scheme file) library            │
     ├───────────────────────────┼────────────┼──────────────────────────────────┤
     │ command-line              │ TODO       │ (scheme process-context) library │
     ├───────────────────────────┼────────────┼──────────────────────────────────┤
     │ exit                      │ TODO       │ (scheme process-context) library │
     ├───────────────────────────┼────────────┼──────────────────────────────────┤
     │ emergency-exit            │ C++        │ (scheme process-context) library │
     ├───────────────────────────┼────────────┼──────────────────────────────────┤
     │ get-environment-variable  │ TODO       │ (scheme process-context) library │
     ├───────────────────────────┼────────────┼──────────────────────────────────┤
     │ get-environment-variables │ TODO       │ (scheme process-context) library │
     ├───────────────────────────┼────────────┼──────────────────────────────────┤
     │ current-second            │ TODO       │ (scheme time) library            │
     ├───────────────────────────┼────────────┼──────────────────────────────────┤
     │ current-jiffy             │ TODO       │ (scheme time) library            │
     ├───────────────────────────┼────────────┼──────────────────────────────────┤
     │ jiffies-per-second        │ TODO       │ (scheme time) library            │
     ├───────────────────────────┼────────────┼──────────────────────────────────┤
     │ features                  │ C++        │                                  │
     └───────────────────────────┴────────────┴──────────────────────────────────┘

    ------------------------------------------------------------------------- */

    define<procedure>("load", [this](let const& xs)
    {
      return load(car(xs).as<const string>());
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


  /* ---- R4RS APPENDIX: A compatible low-level macro facility -------------- */

    define<procedure>("syntactic-closure?", make_predicate<syntactic_closure>());

    define<procedure>("syntactic-continuation?", make_predicate<syntactic_continuation>());

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
    std::stringstream port { overture.data() };

    // std::size_t counts {0};

    for (let e = read(port); e != eof_object; e = read(port))
    {
      // NOTE: THIS WILL NEVER SHOWN (OVERTURE LAYER BOOTS BEFORE CONFIGURATION)
      // write_to(standard_debug_port(),
      //   "\r\x1B[K", header("overture"), counts++, ": ", car(syntactic_environment()));

      evaluate(e);
    }

    // NOTE: THIS WILL NEVER SHOWN (OVERTURE LAYER BOOTS BEFORE CONFIGURATION)
    // write_to(standard_debug_port(), "\n\n");
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
} // namespace kernel
} // namespace meevax
