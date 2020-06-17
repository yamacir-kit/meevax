#ifndef INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CONTINUATION_HPP
#define INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CONTINUATION_HPP

#include <string_view>

#include <meevax/kernel/configurator.hpp>
#include <meevax/kernel/debugger.hpp>
#include <meevax/kernel/linker.hpp>
#include <meevax/kernel/machine.hpp>
#include <meevax/kernel/reader.hpp>
#include <meevax/kernel/writer.hpp>

#include <meevax/kernel/port.hpp>

/* ==== Embedded Source Codes ==================================================
*
* library/hoge.ss
*
* NOTE:
*   readelf -a hoge.ss.o
*
* TODO:
*   Move into new header.
*
*============================================================================ */
extern char _binary_overture_ss_start;
extern char _binary_overture_ss_end;

static const std::string_view overture
{
  &_binary_overture_ss_start,
  static_cast<std::size_t>(&_binary_overture_ss_end - &_binary_overture_ss_start)
};

namespace meevax::kernel
{
  /* ==== Standard Environment Layers ==========================================
  *
  * Layer 0 - Program Structure Controllers
  * Layer 1 - Primitive Expression Types
  * Layer 2 - Derived Expression Types and Standard Procedures
  *
  * ========================================================================= */
  template <auto N>
  static constexpr std::integral_constant<decltype(N), N> layer {};

  /* ==== Syntactic Continuation (SK) ==========================================
  *
  *
  * ========================================================================= */
  class syntactic_continuation

    /* ==== Syntactic Closure ==================================================
     *
     * ====================================================================== */
    : public syntactic_closure

    /* ==== Reader =============================================================
    *
    * Reader access symbol table of this syntactic_continuation (by member
    * function "intern") via static polymorphism. The syntactic_continuation
    * indirectly inherits the non-copyable class std::istream (reader base
    * class), so it cannot be copied.
    *
    * ======================================================================= */
    , public reader<syntactic_continuation>

    /* ==== Writer =============================================================
    *
    *
    * ======================================================================= */
    , public writer<syntactic_continuation>

    /* =========================================================================
    *
    * Each syntactic-continuation has a virtual machine and a compiler.
    *
    * ======================================================================= */
    , public machine<syntactic_continuation>

    /* ==== Debugger ===========================================================
    *
    *
    * ======================================================================= */
    , public debugger<syntactic_continuation>

    /* ==== Configurator =======================================================
    *
    * Global configuration is shared in all of syntactic_continuations running
    * on same process. Thus, any change of configuration member influences any
    * other syntactic_continuations immediately.
    *
    * ======================================================================= */
    , public configurator<syntactic_continuation>
  {
  public:
    std::unordered_map<std::string, object> symbols;
    std::unordered_map<std::string, object> external_symbols; // TODO REMOVE

    std::size_t generation {0};

    using syntactic_closure::syntactic_environment;

    using writer::current_debug_port;
    using writer::current_error_port;
    using writer::current_interaction_port;
    using writer::current_verbose_port;
    using writer::write;
    using writer::write_to;

    using debugger::debug;
    using debugger::header;
    using debugger::indent;
    using debugger::shift;

    using configurator::debugging;
    using configurator::interactive;
    using configurator::quiet;
    using configurator::tracing;
    using configurator::verbose;

  public: // Accessors
    auto current_expression() const -> const auto& { return car(form()); }
    auto scope()              const -> const auto& { return cdr(form()); }

  public: // Constructors
    template <typename... Ts>
    explicit syntactic_continuation(Ts&&... xs)
      : pair { std::forward<decltype(xs)>(xs)... }
    {
      boot(layer<0>);

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

    template <auto N>
    explicit syntactic_continuation(std::integral_constant<decltype(N), N>);

    template <auto N>
    void boot(std::integral_constant<decltype(N), N>)
    {}

  public: // Interfaces
    const auto& intern(const std::string& s)
    {
      if (auto iter {symbols.find(s)}; iter != std::end(symbols))
      {
        return (*iter).second;
      }
      else
      {
        [[maybe_unused]] const auto [position, success] {symbols.emplace(s, make<symbol>(s))};
        return (*position).second;
      }
    }

    const auto& override(const object& variable, const object& environment)
    {
      if (not variable or not environment)
      {
        return variable;
      }
      else if (caar(environment).equivalent_to(variable))
      {
        return caar(environment);
      }
      else
      {
        return override(variable, cdr(environment));
      }
    }

    template <typename T, typename... Ts>
    decltype(auto) define(const std::string& name, Ts&&... xs)
    {
      return
        machine<syntactic_continuation>::define(
          override(intern(name), syntactic_environment()),
          make<T>(name, std::forward<decltype(xs)>(xs)...));
    }

    template <typename... Ts>
    decltype(auto) define(const std::string& name, Ts&&... xs)
    {
      return
        machine<syntactic_continuation>::define(
          override(intern(name), syntactic_environment()),
          std::forward<decltype(xs)>(xs)...);
    }

    std::unordered_map<object, object> renames {};

    auto rename(const object& identifier) -> const auto&
    {
      if (const auto iter { renames.find(identifier) }; iter != std::end(renames))
      {
        return std::get<1>(*iter);
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
            form, // <lambda> parameters
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

    // TODO CONVERT TO VM INSTRUCTION
    decltype(auto) evaluate(const object& expression)
    {
      return
        execute_interrupt(
          compile(expression, syntactic_environment()));
    }

    auto load(const path& path_to_source) -> const auto&
    {
      write_to(current_debug_port(),
        header("loader"), "open ", path_to_source, " => ");

      if (auto port {open_input_file(path_to_source.c_str())}; port)
      {
        write_to(current_debug_port(), t, "\n");

        // push(d, s, e, c);
        // s = e = c = unit;

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
      if (verbose_mode.equivalent_to(t))
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

        std::cerr << ";\t\t; exported identifiers are" << std::endl;

        for ([[maybe_unused]] const auto& [key, value] : external_symbols)
        {
          std::cerr << ";\t\t;   " << value << std::endl;
        }

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

        for ([[maybe_unused]] const auto& [key, value] : xs.as<syntactic_continuation>().external_symbols)
        {
          std::cerr << ";\t\t; importing " << value << std::endl;
        }

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
    friend auto operator<<(std::ostream& os, const syntactic_continuation& sc)
      -> decltype(os)
    {
      return os << console::magenta << "#,("
                << console::green   << "syntactic-continuation"
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

    friend auto operator <<(std::ostream& os, syntactic_continuation& sk)
      -> decltype(os)
    {
      // TODO
      // Evaluate current_expression, and write the evaluation to ostream.

      return
        sk.write_to(os,
          "syntactic_continuation::operator <<(std::ostream&, syntactic_continuation&)\n");
    }
  };

  #define DEFINE_SYNTAX(NAME, RULE)                                            \
  define<syntax>(NAME, [this](auto&&... xs)                                    \
  {                                                                            \
    return RULE(std::forward<decltype(xs)>(xs)...);                            \
  })

  template <>
  void syntactic_continuation::boot(std::integral_constant<decltype(0), 0>)
  {
    define<procedure>("evaluate", [this](auto&& xs) { return evaluate(car(xs)); });

    DEFINE_SYNTAX("export", exportation);
    DEFINE_SYNTAX("import", importation);

    define<procedure>("rename", [this](auto&& xs)
    {
      return rename(xs ? car(xs) : unspecified);
    });
  }

  template <>
  void syntactic_continuation::boot(std::integral_constant<decltype(1), 1>)
  {
    DEFINE_SYNTAX("begin", sequence);
    DEFINE_SYNTAX("call-with-current-continuation", call_cc);
    DEFINE_SYNTAX("define", definition);
    DEFINE_SYNTAX("fork", fork);
    DEFINE_SYNTAX("if", conditional);
    DEFINE_SYNTAX("lambda", lambda);
    DEFINE_SYNTAX("quote", quotation);
    DEFINE_SYNTAX("reference", reference);
    DEFINE_SYNTAX("set!", assignment);

    define<procedure>("load", [this](auto&& xs)
    {
      return load(car(xs).template as<const string>());
    });

    define<procedure>("linker", [](auto&& xs)
    {
      return make<linker>(car(xs).template as<const string>());
    });

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

    define<procedure>("features", [](auto&&...)                 // (scheme base)
    {
      return current_feature;
    });

    define<procedure>("procedure", [](const object& xs)
    {
      const std::string name { cadr(xs).as<string>() };
      return make<procedure>(name, car(xs).as<linker>().link<procedure::signature>(name));
    });

    define<procedure>("read", [this](const object& xs)
    {
      return read(xs ? car(xs).as<input_port>() : current_input_port());
    });

    define<procedure>("write", [](auto&& xs)
    {
      std::cout << car(xs);
      return unspecified;
    });

    #define DEFINE_PREDICATE(NAME, TYPE)                                       \
    define<procedure>(NAME, [](auto&& xs)                                      \
    {                                                                          \
      if (not xs)                                                              \
      {                                                                        \
        return f;                                                              \
      }                                                                        \
      else for (const auto& x : xs)                                            \
      {                                                                        \
        if (not x or not x.template is<TYPE>())                                \
        {                                                                      \
          return f;                                                            \
        }                                                                      \
      }                                                                        \
                                                                               \
      return t;                                                                \
    })

    DEFINE_PREDICATE("symbol?", symbol);
    DEFINE_PREDICATE("syntactic-closure?", syntactic_closure);
    DEFINE_PREDICATE("syntactic-continuation?", syntactic_continuation);

    define<procedure>("syntax", [this](auto&& xs)
    {
      return make<syntactic_closure>(xs ? car(xs) : unspecified, syntactic_environment());
    });
  }

  template <>
  void syntactic_continuation::boot(std::integral_constant<decltype(2), 2>)
  {
    define<procedure>("std::cout", [](auto&& xs)
    {
      for (const auto& x : xs)
      {
        // TODO
        // std::cout << x.display();

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

  #undef DEFINE_SYNTAX

  template <>
  syntactic_continuation::syntactic_continuation(std::integral_constant<decltype(0), 0>)
    : syntactic_continuation::syntactic_continuation {} // boots layer<0>
  {}

  template <auto N>
  syntactic_continuation::syntactic_continuation(std::integral_constant<decltype(N), N>)
    : syntactic_continuation::syntactic_continuation { layer<N - 1> }
  {
    boot(layer<N>);
  }
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CONTINUATION_HPP

