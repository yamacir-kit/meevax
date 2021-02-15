#include <boost/iostreams/device/array.hpp>
#include <boost/iostreams/stream.hpp>
#include <boost/range/adaptor/reversed.hpp>
#include <boost/range/adaptors.hpp>

#include <iterator>
#include <meevax/kernel/basis.hpp>
#include <meevax/kernel/feature.hpp>
#include <meevax/kernel/syntactic_continuation.hpp>
#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace kernel
{
  auto operator >>(std::istream & is, syntactic_continuation & datum) -> std::istream &
  {
    datum.write_to(datum.standard_output_port(),
      "syntactic_continuation::operator >>(std::istream &, syntactic_continuation &)\n");

    datum.write_to(datum.standard_output_port(),
      "read new expression => ", datum.read(is), "\n");

    // sk.write_to(sk.standard_output_port(),
    //   "program == ", sk.program(),
    //   "current_expression is ", sk.current_expression());

    return is;
  }

  auto operator <<(std::ostream & os, syntactic_continuation & datum) -> std::ostream &
  {
    // TODO
    // Evaluate current_expression, and write the evaluation to ostream.

    return datum.write_to(os, "syntactic_continuation::operator <<(std::ostream &, syntactic_continuation &)\n");
  }

  auto operator <<(std::ostream & os, syntactic_continuation const& datum) -> std::ostream &
  {
    return os << magenta << "#,("
              << green << "syntactic-continuation" << reset
              << faint << " #;" << &datum << reset
              << magenta << ")" << reset;
  }

  template class configurator<syntactic_continuation>;
  template class debugger<syntactic_continuation>;
  template class machine<syntactic_continuation>;
  template class reader<syntactic_continuation>;
  template class writer<syntactic_continuation>;

  #define DEFINE_SYNTAX(KEYWORD, TRANSFORMER_SPEC)                             \
  define<syntax>(KEYWORD, [this](auto&&... xs)                                 \
  {                                                                            \
    return TRANSFORMER_SPEC(std::forward<decltype(xs)>(xs)...);                \
  })

  template <>
  void syntactic_continuation::boot(layer<0>)
  {
    DEFINE_SYNTAX("export", exportation);
    DEFINE_SYNTAX("import", importation);

    define<procedure>("set-trace!", [this](auto&& xs)
    {
      return trace_mode = car(xs);
    });
  }

  template <>
  void syntactic_continuation::boot(layer<1>)
  {
    DEFINE_SYNTAX("begin", sequence);
    DEFINE_SYNTAX("call-with-current-continuation", call_cc);
    // DEFINE_SYNTAX("cons", construct);
    DEFINE_SYNTAX("define", definition);
    DEFINE_SYNTAX("fork-with-current-syntactic-continuation", fork);
    DEFINE_SYNTAX("if", conditional);
    DEFINE_SYNTAX("lambda", lambda);
    DEFINE_SYNTAX("quote", quotation);
    DEFINE_SYNTAX("reference", reference);
    DEFINE_SYNTAX("set!", assignment);
  }

  template <>
  void syntactic_continuation::boot(layer<2>)
  {
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
      if (let const& lhs = car(xs), rhs = cadr(xs); eq(lhs, rhs))
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
     │ =                  │ C++        │ Number::operator ==(object const&) │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ <                  │ C++        │ Number::operator < (object const&) │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ >                  │ C++        │ Number::operator > (object const&) │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ <=                 │ C++        │ Number::operator <=(object const&) │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ >=                 │ C++        │ Number::operator >=(object const&) │
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
     │ round              │ C++        │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ rationalize        │ Scheme     │                                    │
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
     │ sqrt               │ C++        │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ exact-integer-sqrt │ TODO       │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ expt               │ C++        │                                    │
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
     -----------------------------------
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

    DEFINE_TRANSITIVE_COMPARISON(= , [](auto&& a, auto&& b) { return a.binding() == b; });
    DEFINE_TRANSITIVE_COMPARISON(< , [](auto&& a, auto&& b) { return a.binding() <  b; });
    DEFINE_TRANSITIVE_COMPARISON(<=, [](auto&& a, auto&& b) { return a.binding() <= b; });
    DEFINE_TRANSITIVE_COMPARISON(> , [](auto&& a, auto&& b) { return a.binding() >  b; });
    DEFINE_TRANSITIVE_COMPARISON(>=, [](auto&& a, auto&& b) { return a.binding() >= b; });

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
        let const basis = make<exact_integer>(BASIS);                          \
                                                                               \
        return std::accumulate(                                                \
                 std::begin(xs), std::end(xs), basis, [](auto&& x, auto&& y)   \
                 {                                                             \
                   return x SYMBOL y;                                          \
                 });                                                           \
      }                                                                        \
      else                                                                     \
      {                                                                        \
        auto const head = std::cbegin(xs);                                     \
                                                                               \
        return std::accumulate(                                                \
                 std::next(head), std::cend(xs), *head, [](auto&& x, auto&& y) \
                 {                                                             \
                   return x SYMBOL y;                                          \
                 });                                                           \
      }                                                                        \
    })

    BOILERPLATE(-, 0);
    BOILERPLATE(/, 1);
    BOILERPLATE(%, 1);

    #undef BOILERPLATE

    define<procedure>("floor"   , [](let const& xs) { return apply_1([](auto&& x          ) { return std::floor(x   ); }, car(xs)          ); });
    define<procedure>("ceiling" , [](let const& xs) { return apply_1([](auto&& x          ) { return std::ceil (x   ); }, car(xs)          ); });
    define<procedure>("truncate", [](let const& xs) { return apply_1([](auto&& x          ) { return std::trunc(x   ); }, car(xs)          ); });
    define<procedure>("round"   , [](let const& xs) { return apply_1([](auto&& x          ) { return std::round(x   ); }, car(xs)          ); });

    define<procedure>( "sin"    , [](let const& xs) { return apply_1([](auto&& x          ) { return std:: sin (x   ); }, car(xs)          ); });
    define<procedure>( "sinh"   , [](let const& xs) { return apply_1([](auto&& x          ) { return std:: sinh(x   ); }, car(xs)          ); });
    define<procedure>("asinh"   , [](let const& xs) { return apply_1([](auto&& x          ) { return std::asinh(x   ); }, car(xs)          ); });
    define<procedure>("asin"    , [](let const& xs) { return apply_1([](auto&& x          ) { return std::asin (x   ); }, car(xs)          ); });

    define<procedure>( "cos"    , [](let const& xs) { return apply_1([](auto&& x          ) { return std:: cos (x   ); }, car(xs)          ); });
    define<procedure>( "cosh"   , [](let const& xs) { return apply_1([](auto&& x          ) { return std:: cosh(x   ); }, car(xs)          ); });
    define<procedure>("acosh"   , [](let const& xs) { return apply_1([](auto&& x          ) { return std::acosh(x   ); }, car(xs)          ); });
    define<procedure>("acos"    , [](let const& xs) { return apply_1([](auto&& x          ) { return std::acos (x   ); }, car(xs)          ); });

    define<procedure>( "tan"    , [](let const& xs) { return apply_1([](auto&& x          ) { return std:: tan (x   ); }, car(xs)          ); });
    define<procedure>( "tanh"   , [](let const& xs) { return apply_1([](auto&& x          ) { return std:: tanh(x   ); }, car(xs)          ); });
    define<procedure>("atanh"   , [](let const& xs) { return apply_1([](auto&& x          ) { return std::atanh(x   ); }, car(xs)          ); });
    define<procedure>("atan-1"  , [](let const& xs) { return apply_1([](auto&& x          ) { return std::atan (x   ); }, car(xs)          ); });
    define<procedure>("atan-2"  , [](let const& xs) { return apply_2([](auto&& y, auto&& x) { return std::atan2(y, x); }, car(xs), cadr(xs)); });

    define<procedure>("sqrt"    , [](let const& xs) { return apply_1([](auto&& x          ) { return std::sqrt (x   ); }, car(xs)          ); });

    define<procedure>("ln"      , [](let const& xs) { return apply_1([](auto&& x          ) { return std::log  (x   ); }, car(xs)          ); });
    define<procedure>("exp"     , [](let const& xs) { return apply_1([](auto&& x          ) { return std::exp  (x   ); }, car(xs)          ); });
    define<procedure>("expt"    , [](let const& xs) { return apply_2([](auto&& x, auto&& y) { return std::pow  (x, y); }, car(xs), cadr(xs)); });

    define<procedure>(  "exact", [](auto&& xs) { return   exact(car(xs)); });
    define<procedure>("inexact", [](auto&& xs) { return inexact(car(xs)); });

    define<procedure>("number->string", [](auto&& xs)
    {
      return make<string>(boost::lexical_cast<std::string>(car(xs)));
    });

    define<procedure>("string->number", [](let const& xs)
    {
      return to_number(car(xs).as<string>(), cdr(xs).is<pair>() ? cadr(xs).as<exact_integer>().to<int>() : 10);
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
      │ caar               │ Scheme     │ SRFI-1                             │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ cadr               │ Scheme     │ SRFI-1                             │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ cdar               │ Scheme     │ SRFI-1                             │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ cddr               │ Scheme     │ SRFI-1                             │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ caaar ... cddddr   │ Scheme     │ SRFI-1                             │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ null?              │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ list?              │ Scheme     │ SRFI-1 (proper-list?)              │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ make-list          │ Scheme     │ SRFI-1                             │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ list               │ Scheme     │ SRFI-1                             │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ length             │ Scheme     │ SRFI-1                             │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ append             │ Scheme     │ SRFI-1                             │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ reverse            │ Scheme     │ SRFI-1                             │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ list-tail          │ Scheme     │ SRFI-1 (drop)                      │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ list-ref           │ Scheme     │ SRFI-1                             │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ list-set!          │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ memq               │ Scheme     │ SRFI-1                             │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ memv               │ Scheme     │ SRFI-1                             │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ member             │ Scheme     │ SRFI-1                             │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ assq               │ Scheme     │ SRFI-1                             │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ assv               │ Scheme     │ SRFI-1                             │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ assoc              │ Scheme     │ SRFI-1                             │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ list-copy          │ Scheme     │ SRFI-1                             │
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
      return make<string>(car(xs).as<symbol>());
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
        return make<exact_integer>(static_cast<codeunit const&>(car(xs).as<character>()));
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
        return make<exact_integer>(static_cast<codepoint>(x.as<character>()));
      }
      else
      {
        throw error(cat("Procedure char-integer got ", xs));
      }
    });

    define<procedure>("integer->char", [](let const& xs)
    {
      if (xs.is<null>())
      {
        throw error(cat("Procedure integer->char got ", xs));
      }
      else if (let const& x = car(xs); x.is<exact_integer>())
      {
        return make<character>(x.as<exact_integer>().to<codepoint>());
      }
      else
      {
        throw error(cat("Procedure integer->char got ", xs));
      }
    });


  /* ---- R7RS 6.7. Strings ----------------------------------------------------

      ┌────────────────────┬────────────┬────────────────────────────────────┐
      │ Symbol             │ Written in │ Note                               │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string?            │ C++        │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ make-string        │ C++        │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string             │ Scheme     │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string-length      │ C++        │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string-ref         │ C++        │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string-set!        │ C++        │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string=?           │ C++        │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string<?           │ C++        │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string>?           │ C++        │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string<=?          │ C++        │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string>=?          │ C++        │                                    │
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
      │ string-append      │ C++        │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string->list       │ C++        │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ list->string       │ C++        │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string-copy        │ C++        │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string-copy!       │ TODO       │                                    │
      ├────────────────────┼────────────┼────────────────────────────────────┤
      │ string-fill!       │ Scheme     │                                    │
      └────────────────────┴────────────┴────────────────────────────────────┘

    ------------------------------------------------------------------------- */

    /* -------------------------------------------------------------------------
     *
     *  (string? obj)                                                 procedure
     *
     *  Returns #t if obj is a string, otherwise returns #f.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("string?", make_predicate<string>());

    /* -------------------------------------------------------------------------
     *
     *  (make-string k)                                               procedure
     *  (make-string k char)                                          procedure
     *
     *  The make-string procedure returns a newly allocated string of length k.
     *  If char is given, then all the characters of the string are initialized
     *  to char , otherwise the contents of the string are unspecified.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("make-string", [](let const& xs)
    {
      return make<string>(car(xs).as<exact_integer>().to<std::size_t>(),
                          cdr(xs).is<pair>() ? cadr(xs).as<character>() : character(' '));
    });

    // NOTE: (string char ...) defined in overture.ss

    /* -------------------------------------------------------------------------
     *
     *  (string-length string)                                        procedure
     *
     *  Returns the number of characters in the given string.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("string-length", [](let const& xs)
    {
      return make<exact_integer>(car(xs).as<string const>().size());
    });

    /* -------------------------------------------------------------------------
     *
     *  (string-ref string k)                                         procedure
     *
     *  It is an error if k is not a valid index of string. The string-ref
     *  procedure returns character k of string using zero-origin indexing.
     *  There is no requirement for this procedure to execute in constant time.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("string-ref", [](let const& xs)
    {
      return make(car(xs).as<string const>().at(cadr(xs).as<exact_integer>().to<string::size_type>()));
    });

    /* -------------------------------------------------------------------------
     *
     *  (string-set! string k char)                                   procedure
     *
     *  It is an error if k is not a valid index of string. The string-set!
     *  procedure stores char in element k of string. There is no requirement
     *  for this procedure to execute in constant time.
     *
     *    (define (f) (make-string 3 #\*))
     *    (define (g) "***")
     *
     *    (string-set! (f) 0 #\?) => unspecified
     *    (string-set! (g) 0 #\?) => error
     *
     *    (string-set! (symbol->string 'immutable) 0 #\?) => error
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("string-set!", [](let const& xs)
    {
      car(xs).as<string>().at(
        cadr(xs).as<exact_integer>().to<string::size_type>())
      = caddr(xs).as<const character>();

      return car(xs);
    });

    /* -------------------------------------------------------------------------
     *
     *  (string=? string1 string2 string3 ...)                       procedure
     *
     *  Returns #t if all the strings are the same length and contain exactly
     *  the same characters in the same positions, otherwise returns #f.
     *
     * ---------------------------------------------------------------------- */

    #define STRING_COMPARE(OPERATOR)                                           \
    [](let const& xs)                                                          \
    {                                                                          \
      for (let const& each : cdr(xs))                                          \
      {                                                                        \
        if (car(xs).as<const string>() OPERATOR each.as<const string>())       \
        {                                                                      \
          continue;                                                            \
        }                                                                      \
        else                                                                   \
        {                                                                      \
          return f;                                                            \
        }                                                                      \
      }                                                                        \
                                                                               \
      return t;                                                                \
    }

    define<procedure>("string=?", STRING_COMPARE(==));

    /* -------------------------------------------------------------------------
     *
     *  (string-ci=? string1 string2 string3 ...)        char library procedure
     *
     *  Returns #t if, after case-folding, all the strings are the same length
     *  and contain the same characters in the same positions, otherwise
     *  returns #f. Specifically, these procedures behave as if string-foldcase
     *  were applied to their arguments before comparing them.
     *
     * ---------------------------------------------------------------------- */
    // TODO

    /* -------------------------------------------------------------------------
     *
     *  (string<? string1 string2 string3 ...)                        procedure
     *  (string-ci<? string1 string2 string3 ...)        char library procedure
     *
     *  (string>? string1 string2 string3 ...)                        procedure
     *  (string-ci>? string1 string2 string3 ...)        char library procedure
     *
     *  (string<=? string1 string2 string3 ...)                       procedure
     *  (string-ci<=? string1 string2 string3 ...)       char library procedure
     *
     *  (string>=? string1 string2 string3 ...)                       procedure
     *  (string-ci>=? string1 string2 string3 ...)       char library procedure
     *
     *  These procedures return #t if their arguments are (respectively):
     *  monotonically increasing, monotonically decreasing, monotonically
     *  non-decreasing, or monotonically nonincreasing. These predicates are
     *  required to be transitive. These procedures compare strings in an
     *  implementation-defined way. One approach is to make them the
     *  lexicographic extensions to strings of the corresponding orderings on
     *  characters. In that case, string<? would be the lexicographic ordering
     *  on strings induced by the ordering char<? on characters, and if the two
     *  strings differ in length but are the same up to the length of the
     *  shorter string, the shorter string would be considered to be
     *  lexicographically less than the longer string. However, it is also
     *  permitted to use the natural ordering imposed by the implementation’s
     *  internal representation of strings, or a more complex locale-specific
     *  ordering.
     *
     *  In all cases, a pair of strings must satisfy exactly one of string<?,
     *  string=?, and string>?, and must satisfy string<=? if and only if they
     *  do not satisfy string>? and string>=? if and only if they do not
     *  satisfy string<?.
     *
     *  The "-ci" procedures behave as if they applied string-foldcase to their
     *  arguments before invoking the corresponding procedures without "-ci".
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("string<?",  STRING_COMPARE(<));
    define<procedure>("string>?",  STRING_COMPARE(>));
    define<procedure>("string<=?", STRING_COMPARE(<=));
    define<procedure>("string>=?", STRING_COMPARE(>=));

    /* -------------------------------------------------------------------------
     *
     *  (string-append string ...)                                    procedure
     *
     *  Returns a newly allocated string whose characters are the concatenation
     *  of the characters in the given strings.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("string-append", [](let const& xs)
    {
      string s;

      for (let const& x : xs)
      {
        std::copy(x.as<const string>().begin(), x.as<const string>().end(), std::back_inserter(s));
      }

      return make(s);
    });

    /* -------------------------------------------------------------------------
     *
     *  (string->list string)                                         procedure
     *  (string->list string start)                                   procedure
     *  (string->list string start end)                               procedure
     *
     *  (list->string list)                                           procedure
     *
     *  It is an error if any element of list is not a character. The
     *  string->list procedure returns a newly allocated list of the characters
     *  of string between start and end. list->string returns a newly allocated
     *  string formed from the elements in the list list. In both procedures,
     *  order is preserved. string->list and list->string are inverses so far
     *  as equal? is concerned.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("string->list", [](let const& xs) // TODO MOVE INTO overture.ss
    {
      let x = unit;

      using boost::adaptors::reverse;
      using boost::adaptors::slice;

      auto start =
        1 < length(xs) ? cadr(xs).as<exact_integer>().to<string::size_type>()
                       : 0;

      auto end =
        2 < length(xs) ? caddr(xs).as<exact_integer>().to<string::size_type>()
                       : car(xs).as<const string>().size();

      for (auto const& each : reverse(slice(car(xs).as<const string>(), start, end)))
      {
        x = cons(make(each), x);
      }

      return x;
    });

    define<procedure>("list->string", [](let const& xs)
    {
      string s;

      for (let const& x : car(xs))
      {
        s.push_back(x.as<const character>());
      }

      return make(std::move(s));
    });

    /* -------------------------------------------------------------------------
     *
     *  (string-copy string)                                          procedure
     *  (string-copy string start)                                    procedure
     *  (string-copy string start end)                                procedure
     *
     *  Returns a newly allocated copy of the part of the given string between
     *  start and end.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("string-copy", [](let const& xs)
    {
      if (cdr(xs).is<null>())
      {
        return make<string>(car(xs).as<string>());
      }
      else if (cddr(xs).is<null>())
      {
        return make<string>(car(xs).as<string>().begin() + cadr(xs).as<exact_integer>().to<string::size_type>(),
                            car(xs).as<string>().end());
      }
      else
      {
        return make<string>(car(xs).as<string>().begin() +  cadr(xs).as<exact_integer>().to<string::size_type>(),
                            car(xs).as<string>().begin() + caddr(xs).as<exact_integer>().to<string::size_type>());
      }
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
     │ vector->string     │ C++        │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ string->vector     │ TODO       │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ vector-copy        │ TODO       │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ vector-copy!       │ TODO       │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ vector-append      │ TODO       │                                    │
     ├────────────────────┼────────────┼────────────────────────────────────┤
     │ vector-fill!       │ C++        │                                    │
     └────────────────────┴────────────┴────────────────────────────────────┘

    ------------------------------------------------------------------------- */

    define<procedure>("vector?", make_predicate<vector>());

    define<procedure>("make-vector", [](let const& xs)
    {
      return make<vector>(car(xs).as<exact_integer>().to<vector::size_type>(),
                          cdr(xs).is<null>() ? unspecified : cadr(xs));
    });

    define<procedure>("vector", [](auto&&... xs)
    {
      return make<vector>(for_each_in, std::forward<decltype(xs)>(xs)...);
    });

    define<procedure>("vector-length", [](let const& xs)
    {
      return make<exact_integer>(car(xs).as<vector>().size());
    });

    define<procedure>("vector-ref", [](let const& xs)
    {
      return car(xs).as<vector>().at(
               cadr(xs).as<exact_integer>().to<vector::size_type>());
    });

    define<procedure>("vector-set!", [](let const& xs)
    {
      return car(xs).as<vector>().at(
               cadr(xs).as<exact_integer>().to<vector::size_type>())
             = caddr(xs);
    });

    define<procedure>("vector->list", [](let const& xs)
    {
      if (let const& v = car(xs); cdr(xs).is<null>())
      {
        return v.as<vector>().to_list();
      }
      else if (let const& from = cadr(xs); cddr(xs).is<null>())
      {
        return v.as<vector>().to_list(from);
      }
      else
      {
        return v.as<vector>().to_list(from, caddr(xs));
      }
    });

    define<procedure>("list->vector", [](let const& xs)
    {
      return make<vector>(for_each_in, car(xs));
    });

    define<procedure>("vector->string", [](let const& xs)
    {
      if (let const& v = car(xs); cdr(xs).is<null>())
      {
        return v.as<vector>().to_string();
      }
      else if (let const& from = cadr(xs); cddr(xs).is<null>())
      {
        return v.as<vector>().to_string(from);
      }
      else
      {
        return v.as<vector>().to_string(from, caddr(xs));
      }
    });

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

    define<procedure>("vector-fill!", [](let const& xs)
    {
      if (let const& v = car(xs), value = cadr(xs); cddr(xs).is<null>())
      {
        return v.as<vector>().fill(value);
      }
      else if (let const& from = caddr(xs); cdddr(xs).is<null>())
      {
        return v.as<vector>().fill(value, from);
      }
      else
      {
        return v.as<vector>().fill(value, from, cadddr(xs));
      }
    });


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
     │ error                  │ TODO       │ SRFI-23                          │
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
      │ open-input-string       │ C++        │ SRFI-6                        │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ open-output-string      │ C++        │ SRFI-6                        │
      ├─────────────────────────┼────────────┼───────────────────────────────┤
      │ get-output-string       │ C++        │ SRFI-6                        │
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

    define<procedure>("input-standard-port", [this](auto&&)
    {
      return standard_input_port();
    });

    define<procedure>("output-standard-port", [this](auto&&)
    {
      return standard_output_port();
    });

    define<procedure>("error-standard-port", [this](auto&&)
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
      else if (let const& x = car(xs); x.is<string>())
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
      return make<string>(car(xs).as<output_string_port>().str());
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
        auto const g = car(xs).as<input_port>().tellg();
        let const c = make<character>(car(xs).as<input_port>());
        car(xs).as<input_port>().seekg(g);
        return c;
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
      car(xs).as<character>().write(cadr(xs).as<output_port>());
      return unspecified;
    });

    define<procedure>("::write-string", [](let const& xs)
    {
      car(xs).as<string>().write_string(cadr(xs).as<output_port>());
      return unspecified;
    });


    define<procedure>("path?", make_predicate<path>());

    define<procedure>("::write-path", [](let const& xs)
    {
      cadr(xs).as<output_port>() << car(xs).as<path>().c_str();
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

    define<procedure>("emergency-exit", [](let const& xs)
    {
      if (xs.is<null>() or not car(xs).is<exact_integer>())
      {
        std::exit(boost::exit_success);
      }
      else
      {
        std::exit(car(xs).as<exact_integer>().to<int>());
      }

      return unspecified;
    });

    define<procedure>("linker", [](auto&& xs)
    {
      return make<linker>(car(xs).template as<const string>());
    });

    define<procedure>("procedure", [](let const& xs)
    {
      std::string const& name = cadr(xs).as<string>();
      return make<procedure>(name, car(xs).as<linker>().link<procedure::signature>(name));
    });

    define<procedure>("features", [](auto&&...)
    {
      return features();
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
    std::vector<string_view> codes {
      values,
      dynamic_wind, // TODO dynamic-wind depends let-values
      srfi_1, // SRFI-1 depends SRFI-8 (receive)
      overture // Derived expression types depends SRFI-1
    };

    for (auto const& code : codes)
    {
      boost::iostreams::stream<boost::iostreams::basic_array_source<char>> port {
        code.begin(), code.size()
      };

      for (let e = read(port); e != eof_object; e = read(port))
      {
        evaluate(e);
      }
    }
  }

  template <>
  void syntactic_continuation::boot(layer<4>)
  {
    define<procedure>("print", [](let const& xs)
    {
      for (let const& x : xs)
      {
        if (x.is<string>())
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
