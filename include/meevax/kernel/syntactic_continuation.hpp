#ifndef INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CONTINUATION_HPP
#define INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CONTINUATION_HPP

#include <meevax/kernel/configurator.hpp>
#include <meevax/kernel/debugger.hpp>
#include <meevax/kernel/machine.hpp>
#include <meevax/kernel/reader.hpp>
#include <meevax/kernel/writer.hpp>

namespace meevax
{
inline namespace kernel
{
  auto decrement = [](auto&& x) constexpr
  {
    return --x;
  };

  /* ---- System Layers --------------------------------------------------------
   *
   * Layer 0 - Module System (Program Structures)
   * Layer 1 - R7RS Primitive Expression Types
   * Layer 2 - R7RS Standard Procedures
   * Layer 3 - Basis Library
   * Layer 4 - Experimental Procedures
   *
   * ------------------------------------------------------------------------ */
  template <std::size_t N>
  using layer = std::integral_constant<decltype(N), N>;

  class syntactic_continuation /* ----------------------------------------------
  *
  *  (<program> . <syntactic environment>)
  *
  * ------------------------------------------------------------------------- */

    : public syntactic_closure /* ----------------------------------------------
    *
    *  (<closure> . <lexical environment>)
    *
    * ----------------------------------------------------------------------- */

    , public reader<syntactic_continuation> /* ---------------------------------
    *
    *  TODO
    *
    * ----------------------------------------------------------------------- */

    , public writer<syntactic_continuation> /* ---------------------------------
    *
    *  TODO
    *
    * ----------------------------------------------------------------------- */

    , public machine<syntactic_continuation> /* --------------------------------
    *
    *  TR-SECD virtual machine and it's compiler.
    *
    * ----------------------------------------------------------------------- */

    , public debugger<syntactic_continuation> /* -------------------------------
    *
    *  TODO
    *
    * ----------------------------------------------------------------------- */

    , public configurator<syntactic_continuation> /* ---------------------------
    *
    *  TODO
    *
    * ----------------------------------------------------------------------- */
  {
  public:
    std::unordered_map<std::string, object> symbols;
    std::unordered_map<std::string, object> external_symbols; // TODO REMOVE

    std::size_t generation = 0;

    using syntactic_closure::syntactic_environment;

    using reader::read;

    using writer::newline;
    using writer::standard_debug_port;
    using writer::standard_error_port;
    using writer::standard_output_port;
    using writer::standard_verbose_port;
    using writer::write;
    using writer::write_to;
    using writer::write_line;

    using configurator::in_batch_mode;
    using configurator::in_debug_mode;
    using configurator::in_interactive_mode;
    using configurator::in_trace_mode;
    using configurator::in_verbose_mode;

  public:
    decltype(auto) current_expression() const
    {
      return car(form());
    }

    decltype(auto) dynamic_environment() const
    {
      return cdr(form());
    }

    auto const& intern(std::string const& s)
    {
      if (auto iter = symbols.find(s); iter != std::end(symbols))
      {
        return cdr(*iter);
      }
      else if (const auto [position, success] = symbols.emplace(s, make<symbol>(s)); success)
      {
        return cdr(*position);
      }
      else
      {
        throw make<error>(make<string>("failed to intern a symbol"), unit);
      }
    }

    template <typename T, typename... Ts>
    decltype(auto) define(std::string const& name, Ts&&... xs)
    {
      return machine<syntactic_continuation>::define(intern(name), make<T>(name, std::forward<decltype(xs)>(xs)...));
    }

    template <typename... Ts>
    decltype(auto) define(std::string const& name, Ts&&... xs)
    {
      return machine<syntactic_continuation>::define(intern(name), std::forward<decltype(xs)>(xs)...);
    }

    decltype(auto) execute()
    {
      static constexpr auto trace = true;

      if (in_trace_mode())
      {
        return machine::execute<trace>();
      }
      else
      {
        return machine::execute();
      }
    }

    decltype(auto) macroexpand(let const& keyword, let const& form)
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

      // for (auto const& each : syntactic_environment())
      // {
      //   std::cout << "  " << each << std::endl;
      // }

      c = current_expression();

      return execute();
    }

    let const evaluate(let const& expression)
    {
      if (in_debug_mode())
      {
        write_to(standard_debug_port(), "\n"); // Blank for compiler's debug-mode prints
      }

      c = compile(in_context_free, syntactic_environment(), expression);

      if (in_debug_mode())
      {
        write_to(standard_debug_port(), "\n");
        disassemble(standard_debug_port().as<output_port>(), c);
      }

      return execute();
    }

    auto load(path const& name) -> auto const&
    {
      write_to(standard_debug_port(), header(__func__), "open ", name, " => ");

      if (let port = make<input_file_port>(name.c_str()); port)
      {
        write_to(standard_debug_port(), t, "\n");

        for (let expression = read(port); expression != eof_object; expression = read(port))
        {
          WRITE_DEBUG(expression);

          evaluate(expression);
        }

        return unspecified;
      }
      else
      {
        write_to(standard_debug_port(), f, "\n");

        throw make<file_error>(make<string>("failed to open file: " + name.string()), unit);
      }
    }

    // XXX DIRTY HACK
    decltype(auto) load(std::string const& name)
    {
      return load(path(name));
    }

    let const& operator [](let const& name)
    {
      return cdr(machine::global(name, syntactic_environment()));
    }

    decltype(auto) operator [](std::string const& name)
    {
      return (*this)[intern(name)];
    }

  public:
    template <typename... Ts>
    explicit syntactic_continuation(Ts &&... xs)
      : pair { std::forward<decltype(xs)>(xs)... }
    {
      boot(layer<0>());

      if (form()) // If called from FORK instruction.
      {
        s = car(form());
        e = cadr(form());
        c = compile(at_the_top_level,
                    syntactic_environment(),
                    caaddr(form()),
                    cdaddr(form()));
        d = cdddr(form());

        form() = execute();

        assert(form().is<closure>());
      }
    }

    template <std::size_t N>
    explicit syntactic_continuation(layer<N>)
      : syntactic_continuation { layer<decrement(N)>() }
    {
      boot(layer<N>());
    }

    template <std::size_t N>
    void boot(layer<N>)
    {}

  public: // Primitive Expression Types
    SYNTAX(exportation)
    {
      if (in_verbose_mode())
      {
        std::cerr << (not indent::depth ? "; compile\t; " : ";\t\t; ")
                  << indent()
                  << expression
                  << faint << " is <export specs>"
                  << reset << std::endl;
      }

      auto exportation = [this](let const& xs)
      {
        for (auto const& each : xs)
        {
          std::cerr << ";\t\t; staging " << each << std::endl;
          external_symbols.emplace(boost::lexical_cast<std::string>(each), each);
        }

        // std::cerr << ";\t\t; exported identifiers are" << std::endl;
        //
        // for ([[maybe_unused]] const auto& [key, value] : external_symbols)
        // {
        //   std::cerr << ";\t\t;   " << value << std::endl;
        // }

        return unspecified;
      };

      return cons(make<instruction>(mnemonic::LOAD_CONSTANT), expression,
                  make<instruction>(mnemonic::LOAD_CONSTANT), make<procedure>("exportation", exportation),
                  make<instruction>(mnemonic::CALL),
                  continuation);
    }

    SYNTAX(importation)
    {
      auto importation = [&](const object& xs)
      {
        assert(xs.is<syntactic_continuation>());

        if (xs.as<syntactic_continuation>().external_symbols.empty())
        {
          std::cerr << "; import\t; " << xs << " is virgin => expand" << std::endl;
          xs.as<syntactic_continuation>().macroexpand(xs, cons(xs, unit));
        }

        // for ([[maybe_unused]] const auto& [key, value] : xs.as<syntactic_continuation>().external_symbols)
        // {
        //   std::cerr << ";\t\t; importing " << value << std::endl;
        // }

        return unspecified;
      };

      // XXX DIRTY HACK
      return reference(in_context_free,
                       syntactic_environment,
                       expression,
                       frames,
                       cons(make<instruction>(mnemonic::LOAD_CONSTANT), make<procedure>("import", importation),
                            make<instruction>(mnemonic::CALL),
                            continuation));
    }
  };

  auto operator >>(std::istream &, syntactic_continuation      &) -> std::istream &;
  auto operator <<(std::ostream &, syntactic_continuation      &) -> std::ostream &;
  auto operator <<(std::ostream &, syntactic_continuation const&) -> std::ostream &;

  extern template class configurator<syntactic_continuation>;
  extern template class debugger<syntactic_continuation>;
  extern template class machine<syntactic_continuation>;
  extern template class reader<syntactic_continuation>;
  extern template class writer<syntactic_continuation>;

  template <> void syntactic_continuation::boot(layer<0>);
  template <> void syntactic_continuation::boot(layer<1>);
  template <> void syntactic_continuation::boot(layer<2>);
  template <> void syntactic_continuation::boot(layer<3>);
  template <> void syntactic_continuation::boot(layer<4>);

  template <>
  syntactic_continuation::syntactic_continuation(layer<0>)
    : syntactic_continuation::syntactic_continuation {}
  {}
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CONTINUATION_HPP
