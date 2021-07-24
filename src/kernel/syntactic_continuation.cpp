#include <boost/cstdlib.hpp>
#include <boost/range/adaptor/reversed.hpp>
#include <boost/range/adaptors.hpp>

#include <ios>
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
    datum.write_to(default_output_port,
      "syntactic_continuation::operator >>(std::istream &, syntactic_continuation &)\n");

    datum.write_to(default_output_port, "read new expression => ", datum.read(is), "\n");

    // sk.write_to(default_output_port,
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

    // TODO (define (set-debug! t/f)
    //        (set! (debug) t/f))
    define<procedure>("set-debug!", [this](auto&&... xs)
    {
      return debug_mode = car(std::forward<decltype(xs)>(xs)...);
    });

    define<procedure>("set-trace!", [this](auto&&... xs)
    {
      return trace_mode = car(std::forward<decltype(xs)>(xs)...);
    });

    define<procedure>("tracker", [](auto&&... xs)
    {
      return make<tracker>(std::forward<decltype(xs)>(xs)...);
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
    DEFINE_SYNTAX("reference", lvalue);
    DEFINE_SYNTAX("set!", assignment);
  }

  template <>
  void syntactic_continuation::boot(layer<2>)
  {
    /* -------------------------------------------------------------------------
     *
     *  (eqv? obj1 obj2)                                              procedure
     *
     *  The eqv? procedure defines a useful equivalence relation on objects.
     *  Briefly, it returns #t if obj1 and obj2 are normally regarded as the
     *  same object.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("eqv?", [](let const& xs)
    {
      let const& a = car(xs);

      auto is_equiv = [&](let const& b)
      {
        return eq(a, b) or a.eqv(b);
      };

      return std::all_of(std::next(std::begin(xs)), std::end(xs), is_equiv) ? t : f;
    });

    /* -------------------------------------------------------------------------
     *
     *  (eq? obj1 obj2)                                               procedure
     *
     *  The eq? procedure is similar to eqv? except that in some cases it is
     *  capable of discerning distinctions finer than those detectable by eqv?.
     *  It must always return #f when eqv? also would, but may return #f in
     *  some cases where eqv? would return #t.
     *
     *  On symbols, booleans, the empty list, pairs, and records, and also on
     *  non-empty strings, vectors, and bytevectors, eq? and eqv? are
     *  guaranteed to have the same behavior. On procedures, eq? must return
     *  true if the arguments' location tags are equal. On numbers and
     *  characters, eq?'s behavior is implementation-dependent, but it will
     *  always return either true or false. On empty strings, empty vectors,
     *  and empty bytevectors, eq? may also behave differently from eqv?.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("eq?", [](auto&& xs)
    {
      return car(xs) == cadr(xs) ? t : f;
    });

    /* -------------------------------------------------------------------------
     *
     *  (number? obj)                                                 procedure
     *  (complex? obj)                                                procedure
     *  (real? obj)                                                   procedure
     *  (rational? obj)                                               procedure
     *  (integer? obj)                                                procedure
     *
     *  These numerical type predicates can be applied to any kind of argument,
     *  including non-numbers. They return #t if the object is of the named
     *  type, and otherwise they return #f. In general, if a type predicate is
     *  true of a number then all higher type predicates are also true of that
     *  number. Consequently, if a type predicate is false of a number, then
     *  all lower type predicates are also false of that number. If z is a
     *  complex number, then (real? z) is true if and only if
     *  (zero? (imag-part z)) is true. If x is an inexact real number, then
     *  (integer? x) is true if and only if (= x (round x)).
     *
     *  The numbers +inf.0, -inf.0, and +nan.0 are real but not rational.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("%complex?", is<complex>());
    define<procedure>("ratio?", is<ratio>());
    define<procedure>("single-float?", is<single_float>());
    define<procedure>("double-float?", is<double_float>());

    /* -------------------------------------------------------------------------
     *
     *  (exact-integer? z)                                            procedure
     *
     *  Returns #t if z is both exact and an integer; otherwise returns #f.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("exact-integer?", is<exact_integer>());

    /* -------------------------------------------------------------------------
     *
     *  (nan? z)                                      inexact library procedure
     *
     *  The nan? procedure returns #t on +nan.0, and on complex numbers if
     *  their real or imaginary parts or both are +nan.0. Otherwise it returns
     *  #f.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("%nan?", [](auto&& xs)
    {
      return std::all_of(std::begin(xs), std::end(xs), is_nan) ? t : f;
    });

    /* -------------------------------------------------------------------------
     *
     *  (=  z1 z2 z3 ...)                                             procedure
     *  (<  x1 x2 x3 ...)                                             procedure
     *  (>  x1 x2 x3 ...)                                             procedure
     *  (<= x1 x2 x3 ...)                                             procedure
     *  (>= x1 x2 x3 ...)                                             procedure
     *
     *  These procedures return #t if their arguments are (respectively):
     *  equal, monotonically increasing, monotonically decreasing,
     *  monotonically non-decreasing, or monotonically non-increasing, and #f
     *  otherwise. If any of the arguments are +nan.0, all the predicates
     *  return #f. They do not distinguish between inexact zero and inexact
     *  negative zero.
     *
     *  These predicates are required to be transitive.
     *
     *  Note: The implementation approach of converting all arguments to
     *  inexact numbers if any argument is inexact is not transitive. For
     *  example, let big be (expt 2 1000), and assume that big is exact and
     *  that inexact numbers are represented by 64-bit IEEE binary floating
     *  point numbers. Then (= (- big 1) (inexact big)) and (= (inexact big)
     *  (+ big 1)) would both be true with this approach, because of the
     *  limitations of IEEE representations of large integers, whereas (= (-
     *  big 1) (+ big 1)) is false. Converting inexact values to exact numbers
     *  that are the same (in the sense of =) to them will avoid this problem,
     *  though special care must be taken with infinities.
     *
     *  Note: While it is not an error to compare inexact numbers using these
     *  predicates, the results are unreliable because a small inaccuracy can
     *  affect the result; this is especially true of = and zero?. When in
     *  doubt, consult a numerical analyst.
     *
     * ---------------------------------------------------------------------- */

    #define BOILERPLATE(SYMBOL, OPERATOR)                                      \
    define<procedure>(#SYMBOL, [](auto&& xs) constexpr                         \
    {                                                                          \
      const auto compare = std::not_fn([](let const& a, let const& b)          \
      {                                                                        \
        return a.load() OPERATOR b;                                            \
      });                                                                      \
                                                                               \
      return std::adjacent_find(                                               \
        std::cbegin(xs), std::cend(xs), compare) == std::end(xs) ? t : f;      \
    })

    BOILERPLATE(= , ==);
    BOILERPLATE(< , < );
    BOILERPLATE(<=, <=);
    BOILERPLATE(> , > );
    BOILERPLATE(>=, >=);

    #undef BOILERPLATE

    /* -------------------------------------------------------------------------
     *
     *  (+ z1 ...)                                                    procedure
     *  (* z1 ...)                                                    procedure
     *
     *  These procedures return the sum or product of their arguments.
     *
     * ---------------------------------------------------------------------- */

    #define BOILERPLATE(SYMBOL, BASIS)                                         \
    define<procedure>(#SYMBOL, [](auto&& xs)                                   \
    {                                                                          \
      return std::accumulate(                                                  \
               std::begin(xs), std::end(xs), BASIS, [](auto&& x, auto&& y)     \
               {                                                               \
                 return x SYMBOL y;                                            \
               });                                                             \
    })

    BOILERPLATE(+, e0);
    BOILERPLATE(*, e1);

    #undef BOILERPLATE

    /* -------------------------------------------------------------------------
     *
     *  (- z)                                                         procedure
     *  (- z1 z2 ...)                                                 procedure
     *  (/ z)                                                         procedure
     *  (/ z1 z2 ...)                                                 procedure
     *
     *  With two or more arguments, these procedures return the difference or
     *  quotient of their arguments, associating to the left. With one
     *  argument, however, they return the additive or multiplicative inverse
     *  of their argument.
     *
     *  It is an error if any argument of / other than the first is an exact
     *  zero. If the first argument is an exact zero, an implementation may
     *  return an exact zero unless one of the other arguments is a NaN.
     *
     * ---------------------------------------------------------------------- */

    #define BOILERPLATE(SYMBOL, BASIS)                                         \
    define<procedure>(#SYMBOL, [](auto&& xs)                                   \
    {                                                                          \
      auto f = [](auto&& x, auto&& y)                                          \
      {                                                                        \
        return x SYMBOL y;                                                     \
      };                                                                       \
                                                                               \
      if (length(xs) < 2)                                                      \
      {                                                                        \
        let const basis = make<exact_integer>(BASIS);                          \
        return std::accumulate(std::cbegin(xs), std::cend(xs), basis, f);      \
      }                                                                        \
      else                                                                     \
      {                                                                        \
        auto const head = std::cbegin(xs);                                     \
        return std::accumulate(std::next(head), std::cend(xs), *head, f);      \
      }                                                                        \
    })

    BOILERPLATE(-, 0);
    BOILERPLATE(/, 1);
    BOILERPLATE(%, 1);

    #undef BOILERPLATE

    /* -------------------------------------------------------------------------
     *
     *  (floor x)                                                     procedure
     *  (ceiling x)                                                   procedure
     *  (truncate x)                                                  procedure
     *  (round x)                                                     procedure
     *
     *  These procedures return integers. The floor procedure returns the
     *  largest integer not larger than x. The ceiling procedure returns the
     *  smallest integer not smaller than x, truncate returns the integer
     *  closest to x whose absolute value is not larger than the absolute value
     *  of x, and round returns the closest integer to x, rounding to even when
     *  x is halfway between two integers.
     *
     * ---------------------------------------------------------------------- */

    #define DEFINE_CMATH_1(NAME, CMATH)                                        \
    define<procedure>(NAME, [](let const& xs)                                  \
    {                                                                          \
      return apply_1([](auto&&... xs)                                          \
      {                                                                        \
        return std::CMATH(std::forward<decltype(xs)>(xs)...);                  \
      }, car(xs));                                                             \
    })

    DEFINE_CMATH_1("floor", floor);
    DEFINE_CMATH_1("ceiling", ceil);
    DEFINE_CMATH_1("truncate", trunc);
    DEFINE_CMATH_1("round", round);

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

    /* -------------------------------------------------------------------------
     *
     *  (inexact z)                                                   procedure
     *  (exact z)                                                     procedure
     *
     *  The procedure inexact returns an inexact representation of z. The value
     *  returned is the inexact number that is numerically closest to the
     *  argument. For inexact arguments, the result is the same as the argument.
     *  For exact complex numbers, the result is a complex number whose real
     *  and imaginary parts are the result of applying inexact to the real and
     *  imaginary parts of the argument, respectively. If an exact argument has
     *  no reasonably close inexact equivalent (in the sense of =), then a
     *  violation of an implementation restriction may be reported.
     *
     *  The procedure exact returns an exact representation of z. The value
     *  returned is the exact number that is numerically closest to the
     *  argument. For exact arguments, the result is the same as the argument.
     *  For inexact nonintegral real arguments, the implementation may return a
     *  rational approximation, or may report an implementation violation. For
     *  inexact complex arguments, the result is a complex number whose real
     *  and imaginary parts are the result of applying exact to the real and
     *  imaginary parts of the argument, respectively. If an inexact argument
     *  has no reasonably close exact equivalent, (in the sense of =), then a
     *  violation of an implementation restriction may be reported.
     *
     *  These procedures implement the natural one-to-one correspondence
     *  between exact and inexact integers throughout an
     *  implementation-dependent range. See section 6.2.3.
     *
     *  Note: These procedures were known in R5RS as exact->inexact and
     *  inexact->exact, respectively, but they have always accepted arguments
     *  of any exactness. The new names are clearer and shorter, as well as
     *  being compatible with R6RS.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("exact", [](auto&& xs)
    {
      return exact(car(xs));
    });

    define<procedure>("inexact", [](auto&& xs)
    {
      return inexact(car(xs));
    });

    /* -------------------------------------------------------------------------
     *
     *  (number->string z)                                            procedure
     *  (number->string z radix)                                      procedure
     *
     *  It is an error if radix is not one of 2, 8, 10, or 16. The procedure
     *  number->string takes a number and a radix and returns as a string an
     *  external representation of the given number in the given radix such
     *  that
     *
     *    (let ((number number)
     *          (radix radix))
     *      (eqv? number (string->number (number->string number radix) radix)))
     *
     *  is true. It is an error if no possible result makes this expression
     *  true. If omitted, radix defaults to 10.
     *
     *  If z is inexact, the radix is 10, and the above expression can be
     *  satisfied by a result that contains a decimal point, then the result
     *  contains a decimal point and is expressed using the minimum number of
     *  digits (exclusive of exponent and trailing zeroes) needed to make the
     *  above expression true [4, 5]; otherwise the format of the result is
     *  unspecified.
     *
     *  The result returned by number->string never contains an explicit radix
     *  prefix.
     *
     *  Note: The error case can occur only when z is not a complex number or
     *  is a complex number with a non-rational real or imaginary part.
     *
     *  Rationale: If z is an inexact number and the radix is 10, then the
     *  above expression is normally satisfied by a result containing a decimal
     *  point. The unspecified case allows for infinities, NaNs, and unusual
     *  representations.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("number->string", [](auto&& xs)
    {
      return make<string>(boost::lexical_cast<std::string>(car(xs)));
    });

    /* -------------------------------------------------------------------------
     *
     *  (string->number string)                                       procedure
     *  (string->number string radix)                                 procedure
     *
     *  Returns a number of the maximally precise representation expressed by
     *  the given string. It is an error if radix is not 2, 8, 10, or 16.
     *
     *  If supplied, radix is a default radix that will be overridden if an
     *  explicit radix prefix is present in string (e.g. "#o177"). If radix is
     *  not supplied, then the default radix is 10. If string is not a
     *  syntactically valid notation for a number, or would result in a number
     *  that the implementation cannot represent, then string->number returns
     *  #f. An error is never signaled due to the content of string.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("string->number", [](let const& xs)
    {
      return to_number(car(xs).as<string>(), cdr(xs).is<pair>() ? cadr(xs).as<exact_integer>().to<int>() : 10);
    });

    /* -------------------------------------------------------------------------
     *
     *  (pair? obj)                                                   procedure
     *
     *  The pair? predicate returns #t if obj is a pair, and otherwise returns
     *  #f.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("pair?", is<pair>());

    /* -------------------------------------------------------------------------
     *
     *  (cons obj1 obj2)                                              procedure
     *
     *  Returns a newly allocated pair whose car is obj1 and whose cdr is obj2.
     *  The pair is guaranteed to be different (in the sense of eqv?) from
     *  every existing object.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("cons", [](auto&& xs)
    {
      return cons(car(xs), cadr(xs));
    });

    /* -------------------------------------------------------------------------
     *
     *  (car pair)                                                    procedure
     *
     *  Returns the contents of the car field of pair. Note that it is an error
     *  to take the car of the empty list.
     *
     *  (cdr pair)                                                    procedure
     *
     *  Returns the contents of the cdr field of pair. Note that it is an error
     *  to take the cdr of the empty list.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("car", [](auto&& xs) { return caar(xs); });
    define<procedure>("cdr", [](auto&& xs) { return cdar(xs); });

    /* -------------------------------------------------------------------------
     *
     *  (set-car! pair obj)                                           procedure
     *
     *  Stores obj in the car field of pair.
     *
     *  (set-cdr! pair obj)                                           procedure
     *
     *  Stores obj in the cdr field of pair.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("set-car!", [](auto&& xs) { return caar(xs) = cadr(xs); });
    define<procedure>("set-cdr!", [](auto&& xs) { return cdar(xs) = cadr(xs); });


    /* -------------------------------------------------------------------------
     *
     *  (symbol? obj)                                                 procedure
     *
     *  Returns #t if obj is a symbol, otherwise returns #f.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("symbol?", is<symbol>());

    /* -------------------------------------------------------------------------
     *
     *  (symbol->string symbol)                                       procedure
     *
     *  Returns the name of symbol as a string, but without adding escapes. It
     *  is an error to apply mutation procedures like string-set! to strings
     *  returned by this procedure.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("symbol->string", [](let const& xs)
    {
      return make<string>(car(xs).as<symbol>());
    });

    /* -------------------------------------------------------------------------
     *
     *  (string->symbol string)                                       procedure
     *
     *  Returns the symbol whose name is string. This procedure can create
     *  symbols with names containing special characters that would require
     *  escaping when written, but does not interpret escapes in its input.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("string->symbol", [](let const& xs)
    {
      return intern(car(xs).as<string>());
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

    define<procedure>("char?", is<character>());

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
      if (xs.is<pair>() and car(xs).is<character>())
      {
        return make<exact_integer>(static_cast<codepoint>(car(xs).as<character>()));
      }
      else
      {
        throw error(make<string>("invalid arguments: "), xs);
      }
    });

    define<procedure>("integer->char", [](let const& xs)
    {
      if (xs.is<pair>() and car(xs).is<exact_integer>())
      {
        return make<character>(car(xs).as<exact_integer>().to<codepoint>());
      }
      else
      {
        throw error(make<string>("invalid arguments: "), xs);
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

    define<procedure>("string?", is<string>());

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
     │ make-vector        │ C++        │                                    │
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

    define<procedure>("vector?", is<vector>());

    define<procedure>("make-vector", [](let const& xs) // TODO Rename to vector-allocate
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

    define<procedure>("native-procedure?", is<procedure>());

    define<procedure>("closure?", is<closure>());

    define<procedure>("continuation?", is<continuation>());


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
     │ error-object?          │ Scheme     │                                  │
     ├────────────────────────┼────────────┼──────────────────────────────────┤
     │ error-object-message   │ Scheme     │                                  │
     ├────────────────────────┼────────────┼──────────────────────────────────┤
     │ error-object-irritants │ Scheme     │                                  │
     ├────────────────────────┼────────────┼──────────────────────────────────┤
     │ read-error?            │ C++        │                                  │
     ├────────────────────────┼────────────┼──────────────────────────────────┤
     │ file-error?            │ C++        │                                  │
     └────────────────────────┴────────────┴──────────────────────────────────┘

    ------------------------------------------------------------------------- */

    define<procedure>("throw", [](let const& xs) -> let
    {
      throw car(xs);
    });

    define<procedure>("make-error", [](let const& xs)
    {
      return make<error>(car(xs), cdr(xs));
    });

    define<procedure>(       "error?", is<       error>());
    define<procedure>(  "read-error?", is<  read_error>());
    define<procedure>(  "file-error?", is<  file_error>());
    define<procedure>("syntax-error?", is<syntax_error>());

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

    define<procedure>("standard-input-port", [](auto&&)
    {
      return default_input_port;
    });

    define<procedure>("standard-output-port", [](auto&&)
    {
      return default_output_port;
    });

    define<procedure>("standard-error-port", [](auto&&)
    {
      return default_error_port;
    });


    define<procedure>("input-file-port?", is<input_file_port>());

    define<procedure>("output-file-port?", is<output_file_port>());

    define<procedure>("input-string-port?", is<input_string_port>());

    define<procedure>("output-string-port?", is<output_string_port>());


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
        throw error(make<string>("not a string"), car(xs));
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
        throw error(make<string>("not a string"), car(xs));
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
      catch (tagged_read_error<eof> const&)
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
      catch (tagged_read_error<eof> const&)
      {
        return eof_object;
      }
    });


    define<procedure>("eof-object?", is<eof>());

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


    define<procedure>("path?", is<path>());

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

    /* -------------------------------------------------------------------------
     *
     *  (emergency-exit)                      process-context library procedure
     *  (emergency-exit obj)                  process-context library procedure
     *
     *  Terminates the program without running any outstanding dynamic-wind
     *  after procedures and communicates an exit value to the operating system
     *  in the same manner as exit.
     *
     *  NOTE: The emergency-exit procedure corresponds to the exit procedure in
     *  Windows and Posix.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("emergency-exit", [](let const& xs) -> let
    {
      if (xs.is<null>() or car(xs) == t)
      {
        throw boost::exit_success;
      }
      else if (let const& x = car(xs); x.is<exact_integer>())
      {
        throw x.as<exact_integer>().to<int>();
      }
      else
      {
        throw boost::exit_failure;
      }
    });

    define<procedure>("features", [](auto&&...)
    {
      return features();
    });


  /* ---- R4RS APPENDIX: A compatible low-level macro facility -------------- */

    define<procedure>("syntactic-continuation?", is<syntactic_continuation>());

    define<procedure>("unwrap-syntax", [](let const& xs)
    {
      if (let const& x = car(xs); x.is<syntactic_continuation>())
      {
        return car(xs).as<syntactic_continuation>().datum;
      }
      else
      {
        return x;
      }
    });

    define<procedure>("macroexpand-1", [this](let const& xs)
    {
      if (let const& macro = (*this)[caar(xs)]; macro.is<syntactic_continuation>())
      {
        return macro.as<syntactic_continuation>().macroexpand(macro, car(xs));
      }
      else
      {
        throw error(make<string>("not a macro"), caar(xs));
      }
    });

    define<procedure>("syntactic-keyword?", is<syntactic_keyword>());

    define<procedure>("identifier->symbol", [](let const& xs)
    {
      return car(xs).as<syntactic_keyword>().unwrap_syntax();
    });

    /* -------------------------------------------------------------------------
     *
     *  (identifier? syntax-object)                                   procedure
     *
     *  Returns #t if syntax-object represents an identifier, otherwise returns
     *  #f.
     *
     * ---------------------------------------------------------------------- */
    define<procedure>("identifier?", [](let const& xs)
    {
      if (let const& x = car(xs); x.is<syntactic_continuation>())
      {
        return x.as<syntactic_continuation>().datum.is<symbol>() ? t : f;
      }
      else
      {
        return x.is<syntactic_keyword>() or x.is<symbol>() ? t : f;
      }
    });
  }

  template <>
  void syntactic_continuation::boot(layer<3>)
  {
    std::vector<string_view> const codes {
      overture,
      srfi_8,
      srfi_1,
      srfi_23,
      srfi_34,
      srfi_39,
      srfi_45,
      srfi_78,
      r7rs,
    };

    for (auto const& code : codes)
    {
      // NOTE: Since read performs a putback operation on a given stream, it must be copied and used.
      std::stringstream port { std::string(code) };

      for (let e = read(port); e != eof_object; e = read(port))
      {
        evaluate(e);
      }
    }
  }

  template <>
  void syntactic_continuation::boot(layer<4>)
  {
    define<procedure>("disassemble", [](let const& xs)
    {
      if (0 < length(xs))
      {
        if (let const& f = car(xs); f.is<closure>())
        {
          disassemble(std::cout, car(f));
        }
      }

      return default_output_port;
    });

    define<procedure>("gc-collect", [](auto&&)
    {
      return make<exact_integer>(gc.collect());
    });

    define<procedure>("gc-count", [](auto&&)
    {
      return make<exact_integer>(gc.count());
    });

    define<procedure>("ieee-float?", [](auto&&)
    {
      return std::numeric_limits<double>::is_iec559 ? t : f;
    });

    define<procedure>("print", [](let const& xs)
    {
      for (let const& x : xs)
      {
        if (x.is<string>())
        {
          std::cout << static_cast<std::string>(x.as<string>());
        }
        else
        {
          std::cout << x;
        }
      }

      std::cout << std::endl;

      return default_output_port;
    });

    /* -------------------------------------------------------------------------
     *
     *  (foreign-function lib*.so function-name)                      procedure
     *
     * ---------------------------------------------------------------------- */
    define<procedure>("foreign-function", [](let const& xs)
    {
      return make<procedure>(cadr(xs).as<string>(), from(car(xs).as<string>()));
    });

    define<procedure>("type-of", [](auto&& xs)
    {
      std::cout << car(xs).type().name() << std::endl;

      return default_output_port;
    });
  }

  static std::size_t count = 0;

  syntactic_continuation::initializer::initializer()
  {
    if (not count++)
    {
      symbols = {};
      external_symbols = {}; // XXX DEPRECATED
    }
  }

  syntactic_continuation::initializer::~initializer()
  {
    if (not --count)
    {
      symbols.clear();
      external_symbols.clear(); // XXX DEPRECATED
    }
  }
} // namespace kernel
} // namespace meevax
