/*
   Copyright 2018-2021 Tatsuya Yamasaki.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

#include <ios>
#include <iterator>

#include <meevax/iostream/lexical_cast.hpp>
#include <meevax/kernel/basis.hpp>
#include <meevax/kernel/syntactic_continuation.hpp>
#include <meevax/kernel/version.hpp>
#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace kernel
{
  template <>
  syntactic_continuation::syntactic_continuation(boot_upto<layer::declarations>)
    : syntactic_continuation::syntactic_continuation {}
  {
    boot<layer::declarations>();
  }

  auto syntactic_continuation::operator [](const_reference name) -> const_reference
  {
    return cdr(machine::locate(name));
  }

  auto syntactic_continuation::operator [](std::string const& name) -> const_reference
  {
    return (*this)[intern(name)];
  }

  auto syntactic_continuation::build() -> void
  {
    /* ---- NOTE -------------------------------------------------------------
     *
     *  If this class was instantiated by the FORK instruction, the instance
     *  will have received the compilation continuation as a constructor
     *  argument.
     *
     *  The car part contains the registers of the virtual Lisp machine
     *  (s e c . d). The cdr part is set to the global environment at the
     *  time the FORK instruction was executed.
     *
     *  Here, the value in the c register is the operand of the FORK
     *  instruction. The operand of the FORK instruction is a pair of a
     *  lambda expression form passed to the syntax fork/csc and a lexical
     *  environment.
     *
     * -------------------------------------------------------------------- */
    if (std::get<0>(*this).is<continuation>())
    {
      /* ---- NOTE -----------------------------------------------------------
       *
       *  If this class is constructed as make<syntactic_continuation>(...),
       *  this object until the constructor is completed, the case noted that
       *  it is the state that is not registered in the GC.
       *
       * ------------------------------------------------------------------ */
      auto const& k = std::get<0>(*this).as<continuation>();

      s = k.s();
      e = k.e();
      c = compile(context::outermost, *this, car(k.c()), cdr(k.c()));
      d = k.d();

      form() = execute();

      assert(form().is<closure>());
    }
    else
    {
      throw error(make<string>(__func__, " was called by something other than the FORK instruction"), unit);
    }
  }

  auto syntactic_continuation::current_expression() const -> const_reference
  {
    return car(form());
  }

  auto syntactic_continuation::define(const_reference name, const_reference value) -> const_reference
  {
    assert(name.is<symbol>());

    return global_environment() = make<identifier>(name, value) | global_environment();
  }

  auto syntactic_continuation::define(std::string const& name, const_reference value) -> const_reference
  {
    return define(intern(name), value);
  }

  auto syntactic_continuation::dynamic_environment() const -> const_reference
  {
    return cdr(form());
  }

  auto syntactic_continuation::evaluate(const_reference expression) -> value_type
  {
    if (is_debug_mode())
    {
      write_to(standard_debug_port(), "\n"); // Blank for compiler's debug-mode prints
    }

    c = compile(context::none, *this, expression);

    if (is_debug_mode())
    {
      write_to(standard_debug_port(), "\n");
      disassemble(standard_debug_port().as<std::ostream>(), c);
    }

    return execute();
  }

  auto syntactic_continuation::execute() -> value_type
  {
    if (is_trace_mode())
    {
      return machine::execute<execution_context::trace>();
    }
    else
    {
      return machine::execute();
    }
  }

  auto syntactic_continuation::fork() const -> value_type
  {
    let const module = make<syntactic_continuation>(current_continuation(), global_environment());

    module.as<syntactic_continuation>().boot();
    module.as<syntactic_continuation>().build();

    return module;
  }

  auto syntactic_continuation::form() const noexcept -> const_reference
  {
    return std::get<0>(*this);
  }

  auto syntactic_continuation::form() noexcept -> reference
  {
    return const_cast<reference>(std::as_const(*this).form());
  }

  auto syntactic_continuation::global_environment() const noexcept -> const_reference
  {
    return std::get<1>(*this);
  }

  auto syntactic_continuation::global_environment() noexcept -> reference
  {
    return const_cast<reference>(std::as_const(*this).global_environment());
  }

  auto syntactic_continuation::load(std::string const& s) -> value_type
  {
    write_to(standard_debug_port(), header(__func__), "open ", s, " => ");

    if (let port = make<input_file_port>(s); port and port.as<input_file_port>().is_open())
    {
      write_to(standard_debug_port(), t, "\n");

      for (let e = read(port); e != eof_object; e = read(port))
      {
        write_to(standard_debug_port(), header(__func__), e, "\n");

        evaluate(e);
      }

      return unspecified;
    }
    else
    {
      write_to(standard_debug_port(), f, "\n");

      throw file_error(make<string>("failed to open file: " + s), unit);
    }
  }

  auto syntactic_continuation::load(const_reference x) -> value_type
  {
    if (x.is<symbol>())
    {
      return load(x.as<symbol>());
    }
    else if (x.is<string>())
    {
      return load(x.as<string>());
    }
    else if (x.is<path>())
    {
      return load(x.as<path>());
    }
    else
    {
      throw file_error(make<string>(string_append(__FILE__, ":", __LINE__, ":", __func__)), unit);
    }
  }

  auto syntactic_continuation::locate(const_reference variable) -> const_reference
  {
    if (let const& binding = assq(variable, global_environment()); eq(binding, f))
    {
      /* -----------------------------------------------------------------------
       *
       *  At the outermost level of a program, a definition
       *
       *      (define <variable> <expression>)
       *
       *  has essentially the same effect as the assignment expression
       *
       *      (set! <variable> <expression>)
       *
       *  if <variable> is bound to a non-syntax value. However, if <variable>
       *  is not bound, or is a syntactic keyword, then the definition will
       *  bind <variable> to a new location before performing the assignment,
       *  whereas it would be an error to perform a set! on an unbound variable.
       *
       * -------------------------------------------------------------------- */

      let const id = make<identifier>(variable);

      cdr(id) = id; // NOTE: Identifier is self-evaluate if is unbound.

      global_environment() = cons(id, global_environment());

      return car(global_environment());
    }
    else
    {
      return binding;
    }
  }

  auto syntactic_continuation::lookup(const_reference variable) const -> const_reference
  {
    if (let const& x = assq(variable, global_environment()); eq(x, f))
    {
      return variable.is<identifier>() ? variable.as<identifier>().symbol() : variable;
    }
    else
    {
      return cdr(x);
    }
  }

  auto syntactic_continuation::macroexpand(const_reference keyword, const_reference form) -> value_type
  {
    ++generation;

    // XXX ???
    push(d, s, e, cons(make<instruction>(mnemonic::STOP), c));

    s = unit;

    // TODO (3)
    // make<procedure>("rename", [this](auto&& xs)
    // {
    //   const auto id { car(xs) };
    //
    //
    // });

    e = cons(
          // form, // <lambda> parameters
          cons(keyword, cdr(form)),
          dynamic_environment());
    // TODO (4)
    // => e = cons(
    //          list(
    //            expression,
    //            make<procedure>("rename", [this](auto&& xs) { ... }),
    //            make<procedure>("compare", [this](auto&& xs) { ... })
    //            ),
    //          dynamic_environment()
    //          );

    // for (auto const& each : global_environment())
    // {
    //   std::cout << "  " << each << std::endl;
    // }

    c = current_expression();

    return execute();
  }

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

  template class machine<syntactic_continuation>;

  template class reader<syntactic_continuation>;

  template class writer<syntactic_continuation>;

  template <>
  void syntactic_continuation::boot<layer::declarations>()
  {
    define<procedure>("free-identifier=?", [this](let const& xs)
    {
      if (let const& a = car(xs); a.is<symbol>() or a.is<identifier>())
      {
        if (let const& b = cadr(xs); b.is<symbol>() or b.is<identifier>())
        {
          if (let const& id1 = a.is<identifier>() ? a.as<identifier>().symbol() : a)
          {
            if (let const& id2 = b.is<identifier>() ? b.as<identifier>().symbol() : b)
            {
              return id1 == id2 ? t : f;
            }
          }
        }
      }

      // if (let const& a = car(xs); a.is<symbol>() or a.is<identifier>())
      // {
      //   if (let const& b = cadr(xs); b.is<symbol>() or b.is<identifier>())
      //   {
      //     if (auto const& id1 = a.is<identifier>() ? a.as<identifier>() : locate(a).as<identifier>(); id1.is_free())
      //     {
      //       if (auto const& id2 = b.is<identifier>() ? b.as<identifier>() : locate(b).as<identifier>(); id2.is_free())
      //       {
      //         return id1 == id2 ? t : f;
      //       }
      //     }
      //   }
      // }

      return f;
    });

    define<syntax>("export", exportation); // XXX DEPRECATED
    define<syntax>("import", importation); // XXX DEPRECATED

    define<procedure>("set-batch!",       [this](auto&&... xs) { return batch       = car(std::forward<decltype(xs)>(xs)...); });
    define<procedure>("set-debug!",       [this](auto&&... xs) { return debug       = car(std::forward<decltype(xs)>(xs)...); });
    define<procedure>("set-interactive!", [this](auto&&... xs) { return interactive = car(std::forward<decltype(xs)>(xs)...); });
    define<procedure>("set-prompt!",      [this](auto&&... xs) { return prompt      = car(std::forward<decltype(xs)>(xs)...); });
    define<procedure>("set-trace!",       [this](auto&&... xs) { return trace       = car(std::forward<decltype(xs)>(xs)...); });
    define<procedure>("set-verbose!",     [this](auto&&... xs) { return verbose     = car(std::forward<decltype(xs)>(xs)...); });
  }

  template <>
  void syntactic_continuation::boot<layer::primitives>()
  {
    define<syntax>("begin", sequence);
    define<syntax>("call-with-current-continuation", call_with_current_continuation);
    define<syntax>("define", definition);
    define<syntax>("fork-with-current-syntactic-continuation", fork_csc);
    define<syntax>("if", conditional);
    define<syntax>("lambda", lambda);
    define<syntax>("letrec", letrec);
    define<syntax>("quote", quotation);
    define<syntax>("set!", assignment);
  }

  template <>
  void syntactic_continuation::boot<layer::standard_procedures>()
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

    #define BOILERPLATE(SYMBOL, FUNCTOR, BASIS)                                \
    define<procedure>(SYMBOL, [](auto&& xs)                                    \
    {                                                                          \
      if (length(xs) < 2)                                                      \
      {                                                                        \
        return std::accumulate(std::begin(xs), std::end(xs), BASIS, FUNCTOR);  \
      }                                                                        \
      else                                                                     \
      {                                                                        \
        auto const head = std::begin(xs);                                      \
        return std::accumulate(std::next(head), std::end(xs), *head, FUNCTOR); \
      }                                                                        \
    })

    BOILERPLATE("-", sub, e0);
    BOILERPLATE("/", div, e1);
    BOILERPLATE("%", mod, e1);

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

    define<procedure>( "sin"  , [](let const& xs) { return apply_1([](auto&& x          ) { return std:: sin (x   ); }, car(xs)          ); });
    define<procedure>( "sinh" , [](let const& xs) { return apply_1([](auto&& x          ) { return std:: sinh(x   ); }, car(xs)          ); });
    define<procedure>("asinh" , [](let const& xs) { return apply_1([](auto&& x          ) { return std::asinh(x   ); }, car(xs)          ); });
    define<procedure>("asin"  , [](let const& xs) { return apply_1([](auto&& x          ) { return std::asin (x   ); }, car(xs)          ); });

    define<procedure>( "cos"  , [](let const& xs) { return apply_1([](auto&& x          ) { return std:: cos (x   ); }, car(xs)          ); });
    define<procedure>( "cosh" , [](let const& xs) { return apply_1([](auto&& x          ) { return std:: cosh(x   ); }, car(xs)          ); });
    define<procedure>("acosh" , [](let const& xs) { return apply_1([](auto&& x          ) { return std::acosh(x   ); }, car(xs)          ); });
    define<procedure>("acos"  , [](let const& xs) { return apply_1([](auto&& x          ) { return std::acos (x   ); }, car(xs)          ); });

    define<procedure>( "tan"  , [](let const& xs) { return apply_1([](auto&& x          ) { return std:: tan (x   ); }, car(xs)          ); });
    define<procedure>( "tanh" , [](let const& xs) { return apply_1([](auto&& x          ) { return std:: tanh(x   ); }, car(xs)          ); });
    define<procedure>("atanh" , [](let const& xs) { return apply_1([](auto&& x          ) { return std::atanh(x   ); }, car(xs)          ); });
    define<procedure>("atan-1", [](let const& xs) { return apply_1([](auto&& x          ) { return std::atan (x   ); }, car(xs)          ); });
    define<procedure>("atan-2", [](let const& xs) { return apply_2([](auto&& y, auto&& x) { return std::atan2(y, x); }, car(xs), cadr(xs)); });

    define<procedure>("sqrt"  , [](let const& xs) { return apply_1([](auto&& x          ) { return std::sqrt (x   ); }, car(xs)          ); });

    define<procedure>("ln"    , [](let const& xs) { return apply_1([](auto&& x          ) { return std::log  (x   ); }, car(xs)          ); });
    define<procedure>("exp"   , [](let const& xs) { return apply_1([](auto&& x          ) { return std::exp  (x   ); }, car(xs)          ); });
    define<procedure>("expt"  , [](let const& xs) { return apply_2([](auto&& x, auto&& y) { return std::pow  (x, y); }, car(xs), cadr(xs)); });

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
      return make<string>(lexical_cast<std::string>(car(xs)));
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
      switch (length(xs))
      {
      case 1:
        return string_to::number(car(xs).as<string>(), 10);

      case 2:
        return string_to::number(car(xs).as<string>(), static_cast<int>(cadr(xs).as<exact_integer>()));

      default:
        throw invalid_application(intern("string->number") | xs);
      }
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

    /* -------------------------------------------------------------------------
     *
     *  (char? obj)                                                   procedure
     *
     *  Returns #t if obj is a character, otherwise returns #f.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("char?", is<character>());

    /* -------------------------------------------------------------------------
     *
     *  (digit-value char)                               char library procedure
     *
     *  This procedure returns the numeric value (0 to 9) of its argument if it
     *  is a numeric digit (that is, if char-numeric? returns #t), or #f on any
     *  other character.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("digit-value", [](let const& xs)
    {
      if (auto c = car(xs).as<character>(); std::isdigit(c.codepoint))
      {
        return make<exact_integer>(c.codepoint - '0');
      }
      else
      {
        return f;
      }
    });

    /* -------------------------------------------------------------------------
     *
     *  (char->integer char)                                          procedure
     *  (integer->char n)                                             procedure
     *
     *  Given a Unicode character, char->integer returns an exact integer
     *  between 0 and #xD7FF or between #xE000 and #x10FFFF which is equal to
     *  the Unicode scalar value of that character. Given a non-Unicode
     *  character, it returns an exact integer greater than #x10FFFF. This is
     *  true independent of whether the implementation uses the Unicode
     *  representation internally.
     *
     *  Given an exact integer that is the value returned by a character when
     *  char->integer is applied to it, integer->char returns that character.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("char->integer", [](let const& xs)
    {
      if (xs.is<pair>() and car(xs).is<character>())
      {
        return make<exact_integer>(car(xs).as<character>().codepoint);
      }
      else
      {
        throw invalid_application(intern("char->integer") | xs);
      }
    });

    define<procedure>("integer->char", [](let const& xs)
    {
      if (xs.is<pair>() and car(xs).is<exact_integer>())
      {
        return make<character>(static_cast<character::value_type>(car(xs).as<exact_integer>()));
      }
      else
      {
        throw invalid_application(intern("integer->char") | xs);
      }
    });

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
      switch (length(xs))
      {
      case 1:
        return make<string>(static_cast<std::size_t>(car(xs).as<exact_integer>()), character());

      case 2:
        return make<string>(static_cast<std::size_t>(car(xs).as<exact_integer>()), cadr(xs).as<character>());

      default:
        throw invalid_application(intern("make-string") | xs);
      }
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
      return make<exact_integer>(car(xs).as<string>().size());
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
      return make(car(xs).as<string>().at(static_cast<string::size_type>(cadr(xs).as<exact_integer>())));
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
      car(xs).as<string>().at(static_cast<string::size_type>(cadr(xs).as<exact_integer>())) = caddr(xs).as<character>();
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
      string result;

      for (let const& x : xs)
      {
        std::copy(std::cbegin(x.as<string>()), std::cend(x.as<string>()), std::back_inserter(result));
      }

      return make(result);
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

    define<procedure>("string->list", [](let const& xs)
    {
      switch (length(xs))
      {
      case 1:
        return car(xs).as<string>().list();

      case 2:
        return car(xs).as<string>().list(static_cast<string::size_type>(cadr(xs).as<exact_integer>()));

      case 3:
        return car(xs).as<string>().list(static_cast<string::size_type>(cadr(xs).as<exact_integer>()), static_cast<string::size_type>(caddr(xs).as<exact_integer>()));

      default:
        throw invalid_application(intern("string->list") | xs);
      }
    });

    define<procedure>("list->string", [](let const& xs)
    {
      string s;

      for (let const& x : car(xs))
      {
        s.push_back(x.as<character>());
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
      switch (length(xs))
      {
      case 1:
        return make<string>(car(xs).as<string>());

      case 2:
        return make<string>(car(xs).as<string>().begin() + static_cast<string::size_type>(cadr(xs).as<exact_integer>()),
                            car(xs).as<string>().end());
      case 3:
        return make<string>(car(xs).as<string>().begin() + static_cast<string::size_type>( cadr(xs).as<exact_integer>()),
                            car(xs).as<string>().begin() + static_cast<string::size_type>(caddr(xs).as<exact_integer>()));
      default:
        throw invalid_application(intern("string-copy") | xs);
      }
    });

    /* -------------------------------------------------------------------------
     *
     *  (vector? obj)                                                 procedure
     *
     *  Returns #t if obj is a vector; otherwise returns #f.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("vector?", is<vector>());

    /* -------------------------------------------------------------------------
     *
     *  (make-vector k)                                               procedure
     *  (make-vector k fill)                                          procedure
     *
     *  Returns a newly allocated vector of k elements. If a second argument is
     *  given, then each element is initialized to fill. Otherwise the initial
     *  contents of each element is unspecified.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("make-vector", [](let const& xs)
    {
      switch (length(xs))
      {
      case 1:
        return make<vector>(static_cast<vector::size_type>(car(xs).as<exact_integer>()), unspecified);

      case 2:
        return make<vector>(static_cast<vector::size_type>(car(xs).as<exact_integer>()), cadr(xs));

      default:
        throw invalid_application(intern("make-vector") | xs);
      }
    });

    /* -------------------------------------------------------------------------
     *
     *  (vector obj ...)                                              procedure
     *
     *  Returns a newly allocated vector whose elements contain the given
     *  arguments. It is analogous to list.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("vector", [](auto&&... xs)
    {
      return make<vector>(for_each_in, std::forward<decltype(xs)>(xs)...);
    });

    /* -------------------------------------------------------------------------
     *
     *  (vector-length vector)                                        procedure
     *
     *  Returns the number of elements in vector as an exact integer.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("vector-length", [](let const& xs)
    {
      return make<exact_integer>(car(xs).as<vector>().size());
    });

    /* -------------------------------------------------------------------------
     *
     *  (vector-ref vector k)                                         procedure
     *
     *  It is an error if k is not a valid index of vector. The vector-ref
     *  procedure returns the contents of element k of vector.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("vector-ref", [](let const& xs)
    {
      return car(xs).as<vector>().at(static_cast<vector::size_type>(cadr(xs).as<exact_integer>()));
    });

    /* -------------------------------------------------------------------------
     *
     *  (vector-set! vector k obj)                                    procedure
     *
     *  It is an error if k is not a valid index of vector. The vector-set!
     *  procedure stores obj in element k of vector.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("vector-set!", [](let const& xs)
    {
      return car(xs).as<vector>().at(static_cast<vector::size_type>(cadr(xs).as<exact_integer>())) = caddr(xs);
    });

    /* -------------------------------------------------------------------------
     *
     *  (vector->list vector)                                         procedure
     *  (vector->list vector start)                                   procedure
     *  (vector->list vector start end)                               procedure
     *
     *  (list->vector list)                                           procedure
     *
     *  The vector->list procedure returns a newly allocated list of the
     *  objects contained in the elements of vector between start and end. The
     *  list->vector procedure returns a newly created vector initialized to
     *  the elements of the list list.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("vector->list", [](let const& xs)
    {
      switch (length(xs))
      {
      case 1:
        return car(xs).as<vector>().list();

      case 2:
        return car(xs).as<vector>().list(static_cast<vector::size_type>(cadr(xs).as<exact_integer>()));

      case 3:
        return car(xs).as<vector>().list(static_cast<vector::size_type>(cadr(xs).as<exact_integer>()), static_cast<vector::size_type>(caddr(xs).as<exact_integer>()));

      default:
        throw invalid_application(intern("vector->list") | xs);
      }
    });

    define<procedure>("list->vector", [](let const& xs)
    {
      return make<vector>(for_each_in, car(xs));
    });

    /* -------------------------------------------------------------------------
     *
     *  (vector->string vector)                                       procedure
     *  (vector->string vector start)                                 procedure
     *  (vector->string vector start end)                             procedure
     *
     *  (string->vector string)                                       procedure
     *  (string->vector string start)                                 procedure
     *  (string->vector string start end)                             procedure
     *
     *  It is an error if any element of vector between start and end is not a
     *  character.
     *
     *  The vector->string procedure returns a newly allocated string of the
     *  objects contained in the elements of vector between start and end. The
     *  string->vector procedure returns a newly created vector initialized to
     *  the elements of the string string between start and end.
     *
     *  In both procedures, order is preserved.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("vector->string", [](let const& xs)
    {
      switch (length(xs))
      {
      case 1:
        return car(xs).as<vector>().string();

      case 2:
        return car(xs).as<vector>().string(static_cast<vector::size_type>(cadr(xs).as<exact_integer>()));

      case 3:
        return car(xs).as<vector>().string(static_cast<vector::size_type>(cadr(xs).as<exact_integer>()), static_cast<vector::size_type>(caddr(xs).as<exact_integer>()));

      default:
        throw invalid_application(intern("vector->string") | xs);
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

    /* -------------------------------------------------------------------------
     *
     *  (vector-fill! vector fill)                                    procedure
     *  (vector-fill! vector fill start)                              procedure
     *  (vector-fill! vector fill start end)                          procedure
     *
     *  The vector-fill! procedure stores fill in the elements of vector
     *  between start and end.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("vector-fill!", [](let const& xs)
    {
      switch (length(xs))
      {
      case 2:
        car(xs).as<vector>().fill(cadr(xs));
        break;

      case 3:
        car(xs).as<vector>().fill(cadr(xs), static_cast<string::size_type>(caddr(xs).as<exact_integer>()));
        break;

      case 4:
        car(xs).as<vector>().fill(cadr(xs), static_cast<string::size_type>(caddr(xs).as<exact_integer>()), static_cast<string::size_type>(cadddr(xs).as<exact_integer>()));
        break;

      default:
        throw invalid_application(intern("vector-fill!") | xs);
      }

      return unspecified;
    });

    /* -------------------------------------------------------------------------
     *
     *  (procedure? obj)                                              procedure
     *
     *  Returns #t if obj is a procedure, otherwise returns #f.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("closure?", is<closure>());

    define<procedure>("continuation?", is<continuation>());

    define<procedure>("foreign-function?", is<procedure>());

    /* -------------------------------------------------------------------------
     *
     *  (with-exception-handler handler thunk)                        procedure
     *
     *  It is an error if handler does not accept one argument. It is also an
     *  error if thunk does not accept zero arguments.
     *
     *  The with-exception-handler procedure returns the results of invoking
     *  thunk. Handler is installed as the current exception handler in the
     *  dynamic environment used for the invocation of thunk.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("default-exception-handler", [](let const& xs) -> let
    {
      throw car(xs);
    });

    /* -------------------------------------------------------------------------
     *
     *  (error message obj ...)                                       procedure
     *
     *  Message should be a string.
     *
     *  Raises an exception as if by calling raise on a newly allocated
     *  implementation-defined object which encapsulates the information
     *  provided by message, as well as any objs, known as the irritants. The
     *  procedure error-object? must return #t on such objects.
     *
     *    (define (error . xs)
     *      (raise (apply make-error xs)))
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("make-error", [](let const& xs)
    {
      return make<error>(car(xs), cdr(xs));
    });

    /* -------------------------------------------------------------------------
     *
     *  (read-error? obj)                                             procedure
     *  (file-error? obj)                                             procedure
     *
     *  Error type predicates. Returns #t if obj is an object raised by the
     *  read procedure or by the inability to open an input or output port on a
     *  file, respectively. Otherwise, it returns #f.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("error?", is<error>());

    define<procedure>("read-error?", is<read_error>());

    define<procedure>("file-error?", is<file_error>());

    define<procedure>("syntax-error?", is<syntax_error>());

    /* -------------------------------------------------------------------------
     *
     *  (eval expr-or-def environment-specifier)         eval library procedure
     *
     *  If expr-or-def is an expression, it is evaluated in the specified
     *  environment and its values are returned. If it is a definition, the
     *  specified identifier(s) are defined in the specified environment,
     *  provided the environment is not immutable. Implementations may extend
     *  eval to allow other objects.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("eval", [](let const& xs)
    {
      return cadr(xs).as<syntactic_continuation>().evaluate(car(xs));
    });

    /* -------------------------------------------------------------------------
     *
     *  (input-port? obj)                                             procedure
     *  (output-port? obj)                                            procedure
     *  (textual-port? obj)                                           procedure
     *  (binary-port? obj)                                            procedure
     *  (port? obj)                                                   procedure
     *
     *  These procedures return #t if obj is an input port, output port,
     *  textual port, binary port, or any kind of port, respectively. Otherwise
     *  they return #f.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("input-file-port?", is<input_file_port>());

    define<procedure>("output-file-port?", is<output_file_port>());

    define<procedure>("input-string-port?", is<input_string_port>());

    define<procedure>("output-string-port?", is<output_string_port>());

    /* -------------------------------------------------------------------------
     *
     *  (input-port-open? port)                                       procedure
     *  (output-port-open? port)                                      procedure
     *
     *  Returns #t if port is still open and capable of performing input or
     *  output, respectively, and #f otherwise.
     *
     * --------------------------------------------------------------------- */

    define<procedure>("input-file-port-open?", [](let const& xs)
    {
      return car(xs).as<input_file_port>().is_open() ? t : f;
    });

    define<procedure>("output-file-port-open?", [](let const& xs)
    {
      return car(xs).as<output_file_port>().is_open() ? t : f;
    });

    /* -------------------------------------------------------------------------
     *
     *  (current-input-port)                                          procedure
     *  (current-output-port)                                         procedure
     *  (current-error-port)                                          procedure
     *
     *  Returns the current default input port, output port, or error port (an
     *  output port), respectively. These procedures are parameter objects,
     *  which can be overridden with parameterize (see section 4.2.6). The
     *  initial bindings for these are implementation-defined textual ports.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("standard-input-port", [](auto&&) { return default_input_port; });

    define<procedure>("standard-output-port", [](auto&&) { return default_output_port; });

    define<procedure>("standard-error-port", [](auto&&) { return default_error_port; });

    /* -------------------------------------------------------------------------
     *
     *  (open-input-file string)                         file library procedure
     *  (open-binary-input-file string)                  file library procedure
     *
     *  Takes a string for an existing file and returns a textual input port or
     *  binary input port that is capable of delivering data from the file. If
     *  the file does not exist or cannot be opened, an error that satisfies
     *  file-error? is signaled.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("open-input-file", [](let const& xs)
    {
      return make<input_file_port>(car(xs).as<string>());
    });

    /* -------------------------------------------------------------------------
     *
     *  (open-output-file string)                        file library procedure
     *  (open-binary-output-file string)                 file library procedure
     *
     *  Takes a string naming an output file to be created and returns a
     *  textual output port or binary output port that is capable of writing
     *  data to a new file by that name. If a file with the given name already
     *  exists, the effect is unspecified. If the file cannot be opened, an
     *  error that satisfies file-error? is signaled.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("open-output-file", [](let const& xs)
    {
      return make<output_file_port>(car(xs).as<string>());
    });

    /* -------------------------------------------------------------------------
     *
     *  (close-port port)                                             procedure
     *  (close-input-port port)                                       procedure
     *  (close-output-port port)                                      procedure
     *
     *  Closes the resource associated with port, rendering the port incapable
     *  of delivering or accepting data. It is an error to apply the last two
     *  procedures to a port which is not an input or output port, respectively.
     *  Scheme implementations may provide ports which are simultaneously input
     *  and output ports, such as sockets; the close-input-port and
     *  close-output-port procedures can then be used to close the input and
     *  output sides of the port independently. These routines have no effect
     *  if the port has already been closed.
     *
     * ---------------------------------------------------------------------- */

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

    /* -------------------------------------------------------------------------
     *
     *  (open-input-string string)                                    procedure
     *
     *  Takes a string and returns a textual input port that delivers
     *  characters from the string. If the string is modified, the effect is
     *  unspecified.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("open-input-string", [](let const& xs)
    {
      switch (length(xs))
      {
      case 0:
        return make<input_string_port>();

      case 1:
        return make<input_string_port>(car(xs).as<string>());

      default:
        throw invalid_application(intern("open-input-string") | xs);
      }
    });

    /* -------------------------------------------------------------------------
     *
     *  (open-output-string)                                          procedure
     *
     *  Returns a textual output port that will accumulate characters for
     *  retrieval by get-output-string.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("open-output-string", [](let const& xs)
    {
      switch (length(xs))
      {
      case 0:
        return make<output_string_port>();

      case 1:
        return make<output_string_port>(car(xs).as<string>());

      default:
        throw invalid_application(intern("open-output-string") | xs);
      }
    });

    /* -------------------------------------------------------------------------
     *
     *  (get-output-string port)                                      procedure
     *
     *  It is an error if port was not created with open-output-string.
     *
     *  Returns a string consisting of the characters that have been output to
     *  the port so far in the order they were output. If the result string is
     *  modified, the effect is unspecified.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("get-output-string", [](let const& xs)
    {
      return make<string>(car(xs).as<output_string_port>().str());
    });

    /* -------------------------------------------------------------------------
     *
     *  (read)                                           read library procedure
     *  (read port)                                      read library procedure
     *
     *  The read procedure converts external representations of Scheme objects
     *  into the objects themselves. That is, it is a parser for the
     *  non-terminal hdatumi (see sections 7.1.2 and 6.4). It returns the next
     *  object parsable from the given textual input port, updating port to
     *  point to the first character past the end of the external
     *  representation of the object.
     *
     *  Implementations may support extended syntax to represent record types
     *  or other types that do not have datum representations.
     *
     *  If an end of file is encountered in the input before any characters are
     *  found that can begin an object, then an end-of-file object is returned.
     *  The port remains open, and further attempts to read will also return an
     *  end-of-file object. If an end of file is encountered after the
     *  beginning of an object’s external representation, but the external
     *  representation is incomplete and therefore not parsable, an error that
     *  satisfies read-error? is signaled.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("::read", [this](let const& xs)
    {
      return read(car(xs));
    });

    /* -------------------------------------------------------------------------
     *
     *  (read-char)                                                   procedure
     *  (read-char port)                                              procedure
     *
     *  Returns the next character available from the textual input port,
     *  updating the port to point to the following character. If no more
     *  characters are available, an end-of-file object is returned.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("::read-char", [](let const& xs)
    {
      try
      {
        return make<character>(car(xs).as<std::istream>());
      }
      catch (tagged_read_error<eof> const&)
      {
        return eof_object;
      }
    });

    /* -------------------------------------------------------------------------
     *
     *  (peek-char)                                                   procedure
     *  (peek-char port)                                              procedure
     *
     *  Returns the next character available from the textual input port, but
     *  without updating the port to point to the following character. If no
     *  more characters are available, an end-of-file object is returned.
     *
     *  Note: The value returned by a call to peek-char is the same as the
     *  value that would have been returned by a call to read-char with the
     *  same port. The only difference is that the very next call to read-char
     *  or peek-char on that port will return the value returned by the
     *  preceding call to peek-char. In particular, a call to peek-char on an
     *  interactive port will hang waiting for input whenever a call to
     *  read-char would have hung.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("::peek-char", [](let const& xs)
    {
      try
      {
        auto const g = car(xs).as<std::istream>().tellg();
        let const c = make<character>(car(xs).as<std::istream>());
        car(xs).as<std::istream>().seekg(g);
        return c;
      }
      catch (tagged_read_error<eof> const&)
      {
        return eof_object;
      }
    });

    /* -------------------------------------------------------------------------
     *
     *  (eof-object? obj)                                             procedure
     *
     *  Returns #t if obj is an end-of-file object, otherwise returns #f. The
     *  precise set of end-of-file objects will vary among implementations, but
     *  in any case no end-of-file object will ever be an object that can be
     *  read in using read.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("eof-object?", is<eof>());

    /* -------------------------------------------------------------------------
     *
     *  (eof-object)                                                  procedure
     *
     *  Returns an end-of-file object, not necessarily unique.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("eof-object", [](auto&&)
    {
      return eof_object;
    });

    /* -------------------------------------------------------------------------
     *
     *  (char-ready?)                                                 procedure
     *  (char-ready? port)                                            procedure
     *
     *  Returns #t if a character is ready on the textual input port and
     *  returns #f otherwise. If char-ready returns #t then the next read-char
     *  operation on the given port is guaranteed not to hang. If the port is
     *  at end of file then char-ready? returns #t.
     *
     *  Rationale: The char-ready? procedure exists to make it possible for a
     *  program to accept characters from interactive ports without getting
     *  stuck waiting for input. Any input editors associated with such ports
     *  must ensure that characters whose existence has been asserted by
     *  char-ready? cannot be removed from the input. If char-ready? were to
     *  return #f at end of file, a port at end of file would be
     *  indistinguishable from an interactive port that has no ready characters.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("::char-ready?", [](let const& xs)
    {
      return car(xs).as<std::istream>() ? t : f;
    });

    /* -------------------------------------------------------------------------
     *
     *  (read-string k)                                               procedure
     *  (read-string k port)                                          procedure
     *
     *  Reads the next k characters, or as many as are available before the end
     *  of file, from the textual input port into a newly allocated string in
     *  left-to-right order and returns the string. If no characters are
     *  available before the end of file, an end-of-file object is returned.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("::read-string", [](let const& xs)
    {
      switch (length(xs))
      {
      case 2:
        return make<string>(cadr(xs).as<std::istream>(), static_cast<string::size_type>(car(xs).as<exact_integer>()));

      default:
        throw invalid_application(intern("read-string") | xs);
      }
    });

    /* -------------------------------------------------------------------------
     *
     *  (write-simple obj)                              write library procedure
     *  (write-simple obj port)                         write library procedure
     *
     *  The write-simple procedure is the same as write, except that shared
     *  structure is never represented using datum labels. This can cause
     *  write-simple not to terminate if obj contains circular structure.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("::write-simple", [this](let const& xs)
    {
      write_to(cadr(xs), car(xs));
      return unspecified;
    });

    /* -------------------------------------------------------------------------
     *
     *  (write-char char)                                             procedure
     *  (write-char char port)                                        procedure
     *
     *  Writes the character char (not an external representation of the
     *  character) to the given textual output port and returns an unspecified
     *  value.
     *
     * --------------------------------------------------------------------- */

    define<procedure>("::write-char", [](let const& xs)
    {
      cadr(xs).as<std::ostream>() << static_cast<std::string>(car(xs).as<character>());
      return unspecified;
    });

    /* -------------------------------------------------------------------------
     *
     *  (write-string string)                                         procedure
     *  (write-string string port)                                    procedure
     *  (write-string string port start)                              procedure
     *  (write-string string port start end)                          procedure
     *
     *  Writes the characters of string from start to end in left-to-right
     *  order to the textual output port.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("::write-string", [](let const& xs)
    {
      switch (length(xs))
      {
      case 2:
        cadr(xs).as<std::ostream>() << static_cast<std::string>(car(xs).as<string>());
        break;

      case 3: // TODO
      case 4: // TODO

      default:
        throw invalid_application(intern("write-string") | xs);
      }

      return unspecified;
    });

    define<procedure>("path?", is<path>());

    define<procedure>("::write-path", [](let const& xs)
    {
      cadr(xs).as<std::ostream>() << car(xs).as<path>().c_str();
      return unspecified;
    });

    /* -------------------------------------------------------------------------
     *
     *  (flush-output-port)                                           procedure
     *  (flush-output-port port)                                      procedure
     *
     *  Flushes any buffered output from the buffer of output-port to the
     *  underlying file or device and returns an unspecified value.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("::flush-output-port", [](let const& xs)
    {
      car(xs).as<std::ostream>() << std::flush;
      return unspecified;
    });

    /* -------------------------------------------------------------------------
     *
     *  (load filename)                                  load library procedure
     *  (load filename environment-specifier)            load library procedure
     *
     *  It is an error if filename is not a string. An implementation-dependent
     *  operation is used to transform filename into the name of an existing
     *  file containing Scheme source code. The load procedure reads
     *  expressions and definitions from the file and evaluates them
     *  sequentially in the environment specified by environment-specifier. If
     *  environment-specifier is omitted, (interaction-environment) is assumed.
     *
     *  It is unspecified whether the results of the expressions are printed.
     *  The load procedure does not affect the values returned by
     *  current-input-port and current-output-port. It returns an unspecified
     *  value.
     *
     *  Rationale: For portability, load must operate on source files. Its
     *  operation on other kinds of files necessarily varies among
     *  implementations.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("load", [this](let const& xs)
    {
      return load(car(xs).as<string>());
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

    define<procedure>("emergency-exit", [](let const& xs) -> value_type
    {
      switch (length(xs))
      {
      case 0:
        throw exit_status::success;

      case 1:
        if (let const& x = car(xs); x.is<boolean>())
        {
          throw if_(x) ? exit_status::success : exit_status::failure;
        }
        else if (x.is<exact_integer>())
        {
          throw exit_status(static_cast<int>(x.as<exact_integer>()));
        }
        [[fallthrough]];

      default:
        throw invalid_application(intern("emergency-exit") | xs);
      }
    });

    /* -------------------------------------------------------------------------
     *
     *  (features)                                                    procedure
     *
     *  Returns a list of the feature identifiers which cond-expand treats as
     *  true. It is an error to modify this list. Here is an example of what
     *  features might return:
     *
     *    (features) =>
     *      (r7rs ratios exact-complex full-unicode gnu-linux little-endian
     *      fantastic-scheme fantastic-scheme-1.0 space-ship-control-system)
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("features", [](auto&&...)
    {
      return features();
    });

    /* -------------------------------------------------------------------------
     *
     *  (identifier? syntax-object)                                   procedure
     *
     *  Returns #t if syntax-object represents an identifier, otherwise returns
     *  #f.
     *
     *    (identifier? (syntax x)) => #t
     *
     *    (identifier? (quote x)) => #f
     *
     *    (identifier? 3) => #f
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("identifier?", [](let const& xs)
    {
      let const& x = car(xs);
      return x.is<identifier>() or x.is<symbol>() ? t : f;
    });

    /* -------------------------------------------------------------------------
     *
     *  (unwrap-syntax syntax-object)                                 procedure
     *
     *  If syntax-object is an identifier, then it is returned unchanged.
     *  Otherwise unwrap-syntax converts the outermost structure of
     *  syntax-object into a data object whose external representation is the
     *  same as that of syntax-object. The result is either an identifier, a
     *  pair whose car and cdr are syntax objects, a vector whose elements are
     *  syntax objects, an empty list, a string, a boolean, a character, or a
     *  number.
     *
     *    (identifier? (unwrap-syntax (syntax x))) => #t
     *
     *    (identifier? (car (unwrap-syntax (syntax (x))))) => #t
     *
     *    (unwrap-syntax (cdr (unwrap-syntax (syntax (x))))) => ()
     *
     * ---------------------------------------------------------------------- */

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

    /* -------------------------------------------------------------------------
     *
     *  (identifier->symbol id)                                       procedure
     *
     *  Returns a symbol representing the original name of id.
     *  Identifier->symbol is used to examine identifiers that appear in
     *  literal contexts, i.e., identifiers that will appear in quoted
     *  structures.
     *
     * ---------------------------------------------------------------------- */

    define<procedure>("identifier->symbol", [](let const& xs)
    {
      switch (length(xs))
      {
      case 1:
        if (let const& x = car(xs); x.is<identifier>())
        {
          return x.as<identifier>().symbol();
        }
        else if (x.is<symbol>())
        {
          return x;
        }
        else [[fallthrough]];

      default:
        throw invalid_application(intern("identifier->symbol") | xs);
      }
    });

    define<procedure>("syntactic-continuation?", is<syntactic_continuation>());

    define<procedure>("r6rs:identifier?", is<identifier>());

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
  }

  template <>
  void syntactic_continuation::boot<layer::standard_libraries>()
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
      srfi_149,
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
  void syntactic_continuation::boot<layer::extensions>()
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
      return make<procedure>(cadr(xs).as<string>(), car(xs).as<string>());
    });

    define<procedure>("type-of", [](auto&& xs)
    {
      std::cout << car(xs).type().name() << std::endl;

      return default_output_port;
    });
  }
} // namespace kernel
} // namespace meevax
