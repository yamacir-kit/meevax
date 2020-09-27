#ifndef INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CONTINUATION_HPP
#define INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CONTINUATION_HPP

#include <meevax/kernel/configurator.hpp>
#include <meevax/kernel/debugger.hpp>
#include <meevax/kernel/library.hpp>
#include <meevax/kernel/machine.hpp>
#include <meevax/kernel/port.hpp>
#include <meevax/kernel/reader.hpp>
#include <meevax/kernel/writer.hpp>

namespace meevax { inline namespace kernel
{
  /* ---- System Layers --------------------------------------------------------
   *
   * Layer 0 - Module System (Program Structures)
   * Layer 1 - R7RS Primitive Expression Types
   * Layer 2 - R7RS Standard Procedures
   * Layer 3 - R7RS Derived Expression Types
   * Layer 4 - Experimental Procedures
   *
   * ------------------------------------------------------------------------ */
  template <std::size_t N>
  using layer = std::integral_constant<decltype(N), N>;

  /* ---- Syntactic Continuation (SK) ------------------------------------------
   *
   * TODO
   *
   * ------------------------------------------------------------------------ */
  class syntactic_continuation

    : public syntactic_closure /* ----------------------------------------------
    *
    * TODO
    *
    * ---------------------------------------------------------------------- */

    , public reader<syntactic_continuation> /* ---------------------------------
    *
    * TODO
    *
    * ----------------------------------------------------------------------- */

    , public writer<syntactic_continuation> /* ---------------------------------
    *
    * TODO
    *
    * ----------------------------------------------------------------------- */

    , public machine<syntactic_continuation> /* --------------------------------
    *
    * Each syntactic-continuation has a virtual machine and a compiler.
    *
    * ----------------------------------------------------------------------- */

    , public debugger<syntactic_continuation> /* -------------------------------
    *
    * TODO
    *
    * ----------------------------------------------------------------------- */

    , public configurator<syntactic_continuation> /* ---------------------------
    *
    * TODO
    *
    * ----------------------------------------------------------------------- */
  {
  public:
    std::unordered_map<std::string, object> symbols;
    std::unordered_map<std::string, object> external_symbols; // TODO REMOVE

    std::size_t generation {0};

    using syntactic_closure::syntactic_environment;

    using reader::read;

    using writer::current_debug_port;
    using writer::current_error_port;
    using writer::current_interaction_port;
    using writer::current_output_port;
    using writer::current_verbose_port;
    using writer::newline;
    using writer::write;
    using writer::write_to;
    using writer::writeln;

    using debugger::debug;
    using debugger::header;
    using debugger::indent;
    using debugger::shift;

    using configurator::in_batch_mode;
    using configurator::in_debug_mode;
    using configurator::in_interactive_mode;
    using configurator::in_trace_mode;
    using configurator::in_verbose_mode;

  public:
    template <typename... Ts>
    explicit syntactic_continuation(Ts&&... xs)
      : pair { std::forward<decltype(xs)>(xs)... }
    {
      boot(layer<0>());

      if (first)
      {
        s = car(first);
        e = cadr(first);
        c = unit;
        d = cdddr(first);

        c = compile( // subprogram
              caaddr(first),
              syntactic_environment(),
              cdaddr(first),
              list(make<instruction>(mnemonic::STOP)),
              as_program_declaration);

        form() = execute();

        assert(form().is<closure>());
      }
    }

    template <std::size_t N>
    explicit syntactic_continuation(layer<N>);

    template <std::size_t N>
    void boot(layer<N>)
    {}

  public:
    auto current_expression() const -> const auto& { return car(form()); }
    auto scope()              const -> const auto& { return cdr(form()); }

    const auto& intern(const std::string& s)
    {
      if (auto iter { symbols.find(s) }; iter != std::end(symbols))
      {
        return cdr(*iter);
      }
      else if (const auto [position, success] { symbols.emplace(s, make<symbol>(s)) }; success)
      {
        return cdr(*position);
      }
      else
      {
        std::stringstream port {};
        port << __FILE__ << ":" << __LINE__;
        throw std::runtime_error { port.str() };
      }
    }

    template <typename T, typename... Ts>
    decltype(auto) define(const std::string& name, Ts&&... xs)
    {
      return
        machine<syntactic_continuation>::define(
          intern(name),
          make<T>(name, std::forward<decltype(xs)>(xs)...));
    }

    template <typename... Ts>
    decltype(auto) define(const std::string& name, Ts&&... xs)
    {
      return
        machine<syntactic_continuation>::define(
          intern(name),
          std::forward<decltype(xs)>(xs)...);
    }

    std::unordered_map<object, object> renames {};

    auto rename(const object& identifier) -> const auto&
    {
      if (const auto iter { renames.find(identifier) }; iter != std::end(renames))
      {
        return cdr(*iter);
      }
      else
      {
        renames.emplace(identifier, make<syntactic_closure>(identifier, syntactic_environment()));
        return renames.at(identifier);
      }
    }

    auto expand(const object& identifier, const object& form)
    {
      renames.emplace(car(form), identifier); // set itself to current-renamer

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
            cons(identifier, cdr(form)),
            scope()); // static environment
      // TODO (4)
      // => e = cons(
      //          list(
      //            expression,
      //            make<procedure>("rename", [this](auto&& xs) { ... }),
      //            make<procedure>("compare", [this](auto&& xs) { ... })
      //            ),
      //          scope()
      //          );

      c = current_expression();

      const auto result { execute() };

      ++generation;

      return result;
    }

    decltype(auto) evaluate(const object& expression)
    {
      push(d,
        std::atomic_exchange(&s, unit),
        std::atomic_exchange(&e, unit),
        std::atomic_exchange(&c, compile(expression, syntactic_environment())));

      write_to(current_debug_port(), "; ", std::string(78, '-'), "\n");
      disassemble(c);
      write_to(current_debug_port(), "; ", std::string(78, '-'), "\n");

      decltype(auto) result { execute() };

      s = pop(d);
      e = pop(d);
      c = pop(d);

      return result;
    }

    auto load(const path& path_to_source) -> const auto&
    {
      write_to(current_debug_port(),
        header("loader"), "open ", path_to_source, " => ");

      if (auto port {open_input_file(path_to_source.c_str())}; port)
      {
        write_to(current_debug_port(), t, "\n");

        push(d,
          std::atomic_exchange(&s, unit),
          std::atomic_exchange(&e, unit),
          std::atomic_exchange(&c, unit));

        for (auto expression {read(port)}; expression != eof_object; expression = read(port))
        {
          write_to(current_debug_port(),
            header("loader"), expression, "\n");

          evaluate(expression);
        }

        s = pop(d);
        e = pop(d);
        c = pop(d);

        return unspecified;
      }
      else
      {
        write_to(current_debug_port(), f, "\n");
        throw evaluation_error { "failed to open file ", path_to_source.c_str() };
      }
    }

    // XXX DIRTY HACK
    decltype(auto) load(const std::string& path_to_source)
    {
      return load(path(path_to_source));
    }

  public: // Primitive Expression Types
    DEFINE_PRIMITIVE_EXPRESSION(exportation)
    {
      if (verbose_mode.eqv(t))
      {
        std::cerr
        << (not depth ? "; compile\t; " : ";\t\t; ")
        << std::string(depth * 2, ' ')
        << expression
        << console::faint << " is <export specs>"
        << console::reset << std::endl;
      }

      auto exportation = [this](const object& xs)
      {
        for (const auto& each : xs)
        {
          std::cerr << ";\t\t; staging " << each << std::endl;

          external_symbols.emplace(
            boost::lexical_cast<std::string>(each),
            each);
        }

        // std::cerr << ";\t\t; exported identifiers are" << std::endl;
        //
        // for ([[maybe_unused]] const auto& [key, value] : external_symbols)
        // {
        //   std::cerr << ";\t\t;   " << value << std::endl;
        // }

        return unspecified;
      };

      return
        cons(
          make<instruction>(mnemonic::LOAD_CONSTANT), expression,
          make<instruction>(mnemonic::LOAD_CONSTANT), make<procedure>("exportation", exportation),
          make<instruction>(mnemonic::CALL),
          continuation);
    }

    DEFINE_PRIMITIVE_EXPRESSION(importation)
    {
      auto importation = [&](const object& xs)
      {
        assert(xs.is<syntactic_continuation>());

        if (xs.as<syntactic_continuation>().external_symbols.empty())
        {
          std::cerr << "; import\t; " << xs << " is virgin => expand" << std::endl;
          xs.as<syntactic_continuation>().expand(xs, cons(xs, unit));
        }

        // for ([[maybe_unused]] const auto& [key, value] : xs.as<syntactic_continuation>().external_symbols)
        // {
        //   std::cerr << ";\t\t; importing " << value << std::endl;
        // }

        return unspecified;
      };

      return
        reference( // XXX DIRTY HACK
          expression,
          syntactic_environment,
          frames,
          cons(
            make<instruction>(mnemonic::LOAD_CONSTANT), make<procedure>("import", importation),
            make<instruction>(mnemonic::CALL),
            continuation));
    }

  public:
    friend auto operator<<(std::ostream& os, const syntactic_continuation& sc) -> decltype(auto)
    {
      return os << console::magenta << "#,("
                << console::green << "syntactic-continuation"
                << console::reset
                << console::faint << " #;" << &sc
                << console::reset
                << console::magenta << ")"
                << console::reset;
    }

    friend auto operator >>(std::istream& is, syntactic_continuation& sk)
      -> decltype(is)
    {
      sk.write_to(sk.standard_output_port(),
        "syntactic_continuation::operator >>(std::istream&, syntactic_continuation&)\n");

      sk.write_to(sk.standard_output_port(),
        "read new expression => ", sk.read(is), "\n");

      // sk.write_to(sk.standard_output_port(),
      //   "program == ", sk.program(),
      //   "current_expression is ", sk.current_expression());
      //
      // NOTE
      // Store the expression new read to 'current_expression'.
      // But, currently above comments cause SEGV.

      return is;
    }

    friend auto operator <<(std::ostream& os, syntactic_continuation& sk) -> decltype(auto)
    {
      // TODO
      // Evaluate current_expression, and write the evaluation to ostream.

      return sk.write_to(os, "syntactic_continuation::operator <<(std::ostream&, syntactic_continuation&)\n");
    }
  };

  template <>
  syntactic_continuation::syntactic_continuation(layer<0>)
    : syntactic_continuation::syntactic_continuation {} // boots layer<0>
  {}

  template <std::size_t N>
  syntactic_continuation::syntactic_continuation(layer<N>)
    : syntactic_continuation::syntactic_continuation { layer<N - 1>() }
  {
    boot(layer<N>());
  }

  #define DEFINE_SYNTAX(IDENTIFIER, TRANSFORMER_SPEC)                          \
  define<syntax>(IDENTIFIER, [this](auto&&... xs)                              \
  {                                                                            \
    return TRANSFORMER_SPEC(std::forward<decltype(xs)>(xs)...);                \
  })

  #define DEFINE_PREDICATE(IDENTIFIER, TYPE)                                   \
  define<procedure>(IDENTIFIER, [](auto&& xs)                                  \
  {                                                                            \
    if (null(xs))                                                              \
    {                                                                          \
      return f;                                                                \
    }                                                                          \
    else                                                                       \
    {                                                                          \
      for (let const & x : xs)                                                 \
      {                                                                        \
        if (null(x) or not x.is<TYPE>())                                       \
        {                                                                      \
          return f;                                                            \
        }                                                                      \
      }                                                                        \
                                                                               \
      return t;                                                                \
    }                                                                          \
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
    /* ==== R7RS 6.1. Equivalence predicates ===================================
     *
     *  eq?
     *  eqv?
     *
     * ====================================================================== */
    define<procedure>("eq?", [](auto&& xs)
    {
      return car(xs) == cadr(xs) ? t : f;
    });

    define<procedure>("eqv?", [](auto&& xs)
    {
      if (const object lhs { car(xs) }, rhs { cadr(xs) }; lhs == rhs)
      {
        return t;
      }
      else if (null(lhs) && null(rhs))
      {
        return t;
      }
      else if (null(lhs) || null(rhs))
      {
        return f;
      }
      else
      {
        return lhs.eqv(rhs) ? t : f;
      }
    });

    /* ==== R7RS 6.2. Numbers ==================================================
     *
     *  number?
     *  complex?
     *  real?
     *  rational?
     *
     *  exact?
     *  inexact?
     *  exact-integer?
     *
     *  finite?
     *  infinite?
     *  nan?
     *
     *  =
     *  <
     *  >
     *  <=
     *  >=
     *
     *  zero?
     *  positive?
     *  negative?
     *  odd?
     *  even?
     *
     *  max
     *  min
     *
     *  +
     *  *
     *  -
     *  /
     *
     *  abs
     *
     *  floor/
     *  floor-quotient
     *  floor-remainder
     *  truncate/
     *  truncate-quotient
     *  truncate-remainder
     *
     *  quotient
     *  remainder
     *  modulo
     *
     *  gcd
     *  lcm
     *
     *  numerator
     *  denominator
     *
     *  floor
     *  ceiling
     *  truncate
     *  round
     *
     *  rationalize
     *
     *  exp
     *  log
     *  sin
     *  cos
     *  tan
     *  asin
     *  acos
     *  atan
     *
     *  square                         ; defined in overture.ss
     *  sqrt (square-root)             ; implemented
     *  exact-integer-sqrt
     *  expt (exponential)
     *
     *  make-rectangular
     *  make-polar
     *  real-part
     *  imag-part
     *  magnitude
     *  angle
     *
     *  inexact
     *  exact
     *
     *  number->string
     *  string->number
     *
     * ====================================================================== */
    DEFINE_PREDICATE("the-complex?", complex); // TODO RENAME
    DEFINE_PREDICATE("ratio?", ratio);
    DEFINE_PREDICATE("exact-integer?", exact_integer);

    DEFINE_PREDICATE("single-float?", floating_point<float>);
    DEFINE_PREDICATE("double-float?", floating_point<double>);

    // TODO Rewrite with STL algorithms
    define<procedure>("=", [](auto&& xs)
    {
      const auto head { std::begin(xs) };

      for (auto iter { std::next(head) }; iter != std::end(xs); ++iter)
      {
        if (const auto result { (*head).binding() == *iter }; result.eqv(f))
        {
          return f;
        }
      }

      return t;
    });

    #define boilerplate(SYMBOL)                                                \
    define<procedure>(#SYMBOL, [](auto&& xs)                                   \
    {                                                                          \
      auto lhs { std::begin(xs) };                                             \
                                                                               \
      for (auto rhs { std::next(lhs) }; rhs != std::end(xs); ++lhs, ++rhs)     \
      {                                                                        \
        if (const auto result { *lhs SYMBOL *rhs }; result.eqv(f))             \
        {                                                                      \
          return f;                                                            \
        }                                                                      \
      }                                                                        \
                                                                               \
      return t;                                                                \
    })

    boilerplate(<);
    boilerplate(<=);
    boilerplate(>);
    boilerplate(>=);

    #undef boilerplate

    #define boilerplate(SYMBOL, BASIS)                                         \
    define<procedure>(#SYMBOL, [](auto&& xs)                                   \
    {                                                                          \
      return std::accumulate(std::begin(xs), std::end(xs), make<exact_integer>(BASIS), [](auto&& x, auto&& y) { return x SYMBOL y; }); \
    })

    boilerplate(+, 0);
    boilerplate(*, 1);

    #undef boilerplate


    #define boilerplate(SYMBOL, BASIS)                                         \
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

    boilerplate(-, 0);
    boilerplate(/, 1);

    #undef boilerplate


    #define boilerplate(NAME, CMATH)                                           \
    define<procedure>(NAME, [&](auto&& xs)                                     \
    {                                                                          \
      if (let const x = car(xs); null(x))                                      \
      {                                                                        \
        return f;                                                              \
      }                                                                        \
      else if (x.is<exact_integer>())                                          \
      {                                                                        \
        if (const floating_point<most_precise> result {                        \
              CMATH(x.as<exact_integer>().value.template convert_to<floating_point<most_precise>::value_type>()) \
            }; result.exact())                                                 \
        {                                                                      \
          return make<exact_integer>(result.to_string());                      \
        }                                                                      \
        else                                                                   \
        {                                                                      \
          return make<floating_point<most_precise>>(result);                   \
        }                                                                      \
      }                                                                        \
      else if (x.is<floating_point<float>>())                                  \
      {                                                                        \
        if (const floating_point<float> result { CMATH(x.as<floating_point<float>>()) }; result.exact()) \
        {                                                                      \
          return make<exact_integer>(result.to_string());                      \
        }                                                                      \
        else                                                                   \
        {                                                                      \
          return make<decltype(result)>(result);                               \
        }                                                                      \
      }                                                                        \
      else if (x.is<floating_point<double>>())                                 \
      {                                                                        \
        if (const floating_point<double> result { CMATH(x.as<floating_point<double>>()) }; result.exact()) \
        {                                                                      \
          return make<exact_integer>(result.to_string());                      \
        }                                                                      \
        else                                                                   \
        {                                                                      \
          return make<decltype(result)>(result);                               \
        }                                                                      \
      }                                                                        \
      else                                                                     \
      {                                                                        \
        return f;                                                              \
      }                                                                        \
    })

    boilerplate("exp", std::exp);

    boilerplate("sin", std::sin);
    boilerplate("cos", std::cos);
    boilerplate("tan", std::tan);

    boilerplate("asin", std::asin);
    boilerplate("acos", std::acos);
    boilerplate("atan", std::atan);

    boilerplate("sinh", std::sinh);
    boilerplate("cosh", std::cosh);
    boilerplate("tanh", std::tanh);

    boilerplate("asinh", std::asinh);
    boilerplate("acosh", std::acosh);
    boilerplate("atanh", std::atanh);

    boilerplate("sqrt", std::sqrt);

    #undef boilerplate

    // TODO atan & atan2

    define<procedure>("expt", [](auto&& xs)
    {
      auto inexact = [](const object& z)
      {
        if (z.is<exact_integer>())
        {
          return floating_point<most_precise>(z.as<exact_integer>().to_string()).value;
        }
        else if (z.is<floating_point<float>>())
        {
          return static_cast<floating_point<most_precise>::value_type>(z.as<floating_point<float>>().value);
        }
        else if (z.is<floating_point<double>>())
        {
          return static_cast<floating_point<most_precise>::value_type>(z.as<floating_point<double>>().value);
        }
        else
        {
          return static_cast<floating_point<most_precise>::value_type>(0);
        }
      };

      if (const floating_point<most_precise> result { std::pow(inexact(car(xs)), inexact(cadr(xs))) }; result.exact())
      {
        return make<exact_integer>(result.value);
      }
      else
      {
        return make<decltype(result)>(result);
      }
    });

    define<procedure>("string->number", [](auto&& xs)
    {
      return make_number(car(xs).template as<string>());
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

    define<procedure>("char->integer", [](auto&& xs)
    {
      switch (const auto& s { car(xs).template as<character>().display() }; s.size())
      {
      case 1:
        return make<exact_integer>(*reinterpret_cast<const std::uint8_t*>(s.data()));

      default:
        throw make<evaluation_error>("unicode unsupported");
      }
    });

    /* ==== R7RS 6.7. Strings ==================================================
     *
     *
     * ====================================================================== */
    DEFINE_PREDICATE("string?", string);

    define<procedure>("ccons", [](auto&& xs)
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
        std::stringstream port {};
        port << __FILE__ << ":" << __LINE__;
        throw std::runtime_error { port.str() };
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

      if (car(xs).template is<exact_integer>())
      {
        v.as<vector>().resize(
          static_cast<vector::size_type>(
            car(xs).template as<exact_integer>().value));
      }
      else
      {
        throw std::runtime_error {"type-error"};
      }

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
    define<procedure>("write-" SUFFIX, [this](auto&& xs)                       \
    {                                                                          \
      car(xs).template as<TYPENAME>().display_to(null(cdr(xs)) ? current_output_port() : cadr(xs).template as<output_port>()); \
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

    define<procedure>("emergency-exit", [](auto&& xs)
    {
      if (null(xs) or not car(xs).template is<exact_integer>())
      {
        std::exit(boost::exit_success);
      }
      else
      {
        std::exit(car(xs).template as<exact_integer>().value.template convert_to<int>());
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

    for (auto e {read(port)}; e != eof_object; e = read(port))
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
  }

  #undef DEFINE_PREDICATE
  // #undef DEFINE_SYNTAX
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CONTINUATION_HPP
