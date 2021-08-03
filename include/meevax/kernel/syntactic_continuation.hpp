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
    static inline std::unordered_map<std::string, let> symbols;

    static inline std::unordered_map<std::string, let> external_symbols; // TODO REMOVE

    std::size_t generation = 0;

    let datum = unit;

    // struct initializer
    // {
    //   explicit initializer();
    //
    //   ~initializer();
    // };

    using configurator::is_batch_mode;
    using configurator::is_debug_mode;
    using configurator::is_interactive_mode;
    using configurator::is_trace_mode;
    using configurator::is_verbose_mode;

    using reader::read;

    using writer::newline;
    using writer::standard_debug_port;
    using writer::standard_verbose_port;
    using writer::write;
    using writer::write_to;
    using writer::write_line;

    template <auto Layer>
    explicit syntactic_continuation(boot_upto<Layer>)
      : syntactic_continuation { boot_upto<underlying_decrement(Layer)>() }
    {
      boot<Layer>();
    }

    auto operator [](const_reference) -> const_reference;

    auto operator [](std::string const&) -> const_reference;

    template <auto = layer::declarations>
    void boot();

    auto build() -> void; // NOTE: Only fork() may call this function.

    auto current_expression() const -> const_reference;

    auto define(const_reference, const_reference) -> const_reference;

    auto define(std::string const&, const_reference) -> const_reference;

    template <typename T, typename... Ts>
    auto define(std::string const& name, Ts&&... xs) -> const_reference
    {
      return define(intern(name), make<T>(name, std::forward<decltype(xs)>(xs)...));
    }

    auto dynamic_environment() const -> const_reference;

    auto evaluate(const_reference) -> value_type;

    auto execute() -> value_type;

    auto fork() const -> value_type;

    auto form() const noexcept -> const_reference;

    auto form() noexcept -> reference;

    auto global_environment() const noexcept -> const_reference;

    auto global_environment() noexcept -> reference;

    // TODO MOVE INTO reader
    static auto intern(std::string const& s) -> const_reference
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

    auto load(std::string const&) -> value_type;

    auto load(const_reference) -> value_type;

    auto macroexpand(const_reference, const_reference) -> value_type;

  private:
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
  syntactic_continuation::syntactic_continuation(boot_upto<layer::declarations>);

  template <> auto syntactic_continuation::boot<layer::declarations       >() -> void;
  template <> auto syntactic_continuation::boot<layer::primitives         >() -> void;
  template <> auto syntactic_continuation::boot<layer::standard_procedures>() -> void;
  template <> auto syntactic_continuation::boot<layer::standard_libraries >() -> void;
  template <> auto syntactic_continuation::boot<layer::extensions         >() -> void;

  auto operator >>(std::istream &, syntactic_continuation &) -> std::istream &;

  auto operator <<(std::ostream &, syntactic_continuation &) -> std::ostream &;

  auto operator <<(std::ostream &, syntactic_continuation const&) -> std::ostream &;

  extern template class configurator<syntactic_continuation>;

  extern template class debugger<syntactic_continuation>;

  extern template class machine<syntactic_continuation>;

  extern template class reader<syntactic_continuation>;

  extern template class writer<syntactic_continuation>;

  // static syntactic_continuation::initializer initializer;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CONTINUATION_HPP
