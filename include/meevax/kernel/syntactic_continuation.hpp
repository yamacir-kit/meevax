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
  enum class layer : std::size_t
  {
    declarations,        // 5.
    primitives,          // 4.1.
    standard_procedures, // 6.
    standard_libraries,  // Appendix A
    extensions,
  };

  template <auto Value>
  using boot_upto = typename std::integral_constant<decltype(Value), Value>;

  class syntactic_continuation
    : public virtual pair
    , public configurator <syntactic_continuation>
    , public debugger     <syntactic_continuation>
    , public machine      <syntactic_continuation>
    , public reader       <syntactic_continuation>
    , public writer       <syntactic_continuation>
  {
  public:
    struct initializer
    {
      explicit initializer();

      ~initializer();
    };

    static inline std::unordered_map<std::string, let> symbols;

    static inline std::unordered_map<std::string, let> external_symbols; // TODO REMOVE

    std::size_t generation = 0;

    let datum = unit;

    using reader::read;

    using writer::newline;
    using writer::standard_debug_port;
    using writer::standard_verbose_port;
    using writer::write;
    using writer::write_to;
    using writer::write_line;

    using configurator::is_batch_mode;
    using configurator::is_debug_mode;
    using configurator::is_interactive_mode;
    using configurator::is_trace_mode;
    using configurator::is_verbose_mode;

  public:
    auto build() // NOTE: Only FORK instructions may execute this function.
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
        // let const backup = cons(std::get<0>(*this),
        //                         std::get<1>(*this));

        auto const& k = std::get<0>(*this).as<continuation>();

        s = k.s();
        e = k.e();
        c = compile(at_the_top_level, *this, car(k.c()), cdr(k.c()));
        d = k.d();

        form() = execute();

        assert(form().is<closure>());
      }
    }

    auto form()               const noexcept -> let const& { return std::get<0>(*this); }
    auto form()                     noexcept -> let      & { return std::get<0>(*this); }

    auto global_environment() const noexcept -> let const& { return std::get<1>(*this); }
    auto global_environment()       noexcept -> let      & { return std::get<1>(*this); }

    auto current_expression() const -> decltype(auto)
    {
      return car(form());
    }

    auto dynamic_environment() const -> decltype(auto)
    {
      return cdr(form());
    }

    let static const& intern(std::string const& s)
    {
      if (auto const iter = symbols.find(s); iter != std::end(symbols))
      {
        return cdr(*iter);
      }
      else if (auto const [position, success] = symbols.emplace(s, make<symbol>(s)); success)
      {
        return cdr(*position);
      }
      else
      {
        throw error(make<string>("failed to intern a symbol"), unit);
      }
    }

    template <typename T, typename... Ts>
    auto define(std::string const& name, Ts&&... xs)
    {
      return machine<syntactic_continuation>::define(intern(name), make<T>(name, std::forward<decltype(xs)>(xs)...));
    }

    template <typename... Ts>
    auto define(std::string const& name, Ts&&... xs)
    {
      return machine<syntactic_continuation>::define(intern(name), std::forward<decltype(xs)>(xs)...);
    }

    auto execute() -> let const
    {
      static constexpr auto trace = true;

      if (is_trace_mode())
      {
        return machine::execute<trace>();
      }
      else
      {
        return machine::execute();
      }
    }

    auto macroexpand(let const& keyword, let const& form)
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

    auto evaluate(let const& expression)
    {
      if (is_debug_mode())
      {
        write_to(standard_debug_port(), "\n"); // Blank for compiler's debug-mode prints
      }

      c = compile(in_context_free, *this, expression);

      if (is_debug_mode())
      {
        write_to(standard_debug_port(), "\n");
        disassemble(standard_debug_port().as<output_port>(), c);
      }

      return execute();
    }

    auto load(std::string const& s)
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

    auto load(let const& x)
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

    let const& operator [](let const& name)
    {
      return cdr(machine::locate(name, global_environment()));
    }

    decltype(auto) operator [](std::string const& name)
    {
      return (*this)[intern(name)];
    }

  private:
    /* ---- NOTE ---------------------------------------------------------------
     *
     *  If this class is constructed as make<syntactic_continuation>(...) then
     *  the heterogeneous::binder will have forwarded all constructor arguments
     *  to the virtual base class pair in advance, and this constructor will be
     *  called without any arguments.
     *
     *  (See the heterogeneous::binder::binder for details)
     *
     * ---------------------------------------------------------------------- */
    using pair::pair;

  public:
    template <auto Layer>
    explicit syntactic_continuation(boot_upto<Layer>) // if (0 < layer)
      : syntactic_continuation { boot_upto<underlying_decrement(Layer)>() }
    {
      boot<Layer>();
    }

    template <auto = layer::declarations>
    void boot()
    {}

  public:
    SYNTAX(exportation)
    {
      if (is_verbose_mode())
      {
        std::cerr << (not indent::depth ? "; compile\t; " : ";\t\t; ")
                  << indent()
                  << expression
                  << faint << " is <export specs>"
                  << reset << std::endl;
      }

      auto exportation = [](let const& xs)
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
      auto importation = [&](let const& xs)
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
      return lvalue(in_context_free,
                    current_syntactic_continuation,
                    expression,
                    frames,
                    cons(make<instruction>(mnemonic::LOAD_CONSTANT), make<procedure>("import", importation),
                         make<instruction>(mnemonic::CALL),
                         continuation));
    }
  };

  template <>
  syntactic_continuation::syntactic_continuation(boot_upto<layer::declarations>) // if (layer == 0)
    : syntactic_continuation::syntactic_continuation {}
  {}

  auto operator >>(std::istream &, syntactic_continuation &) -> std::istream &;

  auto operator <<(std::ostream &, syntactic_continuation &) -> std::ostream &;

  auto operator <<(std::ostream &, syntactic_continuation const&) -> std::ostream &;

  extern template class configurator <syntactic_continuation>;
  extern template class debugger     <syntactic_continuation>;
  extern template class machine      <syntactic_continuation>;
  extern template class reader       <syntactic_continuation>;
  extern template class writer       <syntactic_continuation>;

  template <> void syntactic_continuation::boot<layer::declarations       >();
  template <> void syntactic_continuation::boot<layer::primitives         >();
  template <> void syntactic_continuation::boot<layer::standard_procedures>();
  template <> void syntactic_continuation::boot<layer::standard_libraries >();
  template <> void syntactic_continuation::boot<layer::extensions         >();

  static syntactic_continuation::initializer initializer;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CONTINUATION_HPP
