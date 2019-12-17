#ifndef INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CONTINUATION_HPP
#define INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CONTINUATION_HPP

#include <algorithm> // std::equal
#include <numeric> // std::accumulate

// Global configuration generated by CMake before compilation.
#include <meevax/kernel/configurator.hpp>
#include <meevax/kernel/machine.hpp>
#include <meevax/kernel/reader.hpp>
#include <meevax/kernel/file.hpp>
#include <meevax/posix/linker.hpp>

/* ==== Embedded Source Codes ==================================================
*
* library/layer-1.ss
*
* MEMO: readelf -a layer-1.ss.o
*
*============================================================================ */
extern char _binary_layer_1_ss_start;
extern char _binary_layer_1_ss_end;

namespace meevax::kernel
{
  /* ==== Standard Environment Layers ==========================================
  *
  * Layer 0 - Program Structure Controllers
  * Layer 1 - Primitive Expression Types
  * Layer 2 - Derived Expression Types and Standard Procedures
  *
  *========================================================================== */
  template <int Layer>
  static constexpr std::integral_constant<int, Layer> layer {};

  static constexpr auto only_program_structure_controllers {layer<0>};
  static constexpr auto only_primitive_expression_types    {layer<1>};

  class syntactic_continuation
    /* =========================================================================
    *
    * The syntactic-continuation is a pair of "the program" and "global
    * environment (simple association list)". It also has the aspect of a
    * meta-closure that closes the global environment when it constructed (this
    * feature is known as syntactic-closure).
    *
    *======================================================================== */
    : public virtual pair

    /* =========================================================================
    *
    * Reader access symbol table of this syntactic_continuation (by member
    * function "intern") via static polymorphism. The syntactic_continuation
    * indirectly inherits the non-copyable class std::istream (reader base
    * class), so it cannot be copied.
    *
    *======================================================================== */
    , public reader<syntactic_continuation>

    /* =========================================================================
    *
    * Each syntactic-continuation has a virtual machine and a compiler.
    *
    *======================================================================== */
    , public machine<syntactic_continuation>

    /* =========================================================================
    *
    * Global configuration is shared in all of syntactic_continuations running
    * on same process. Thus, any change of configuration member influences any
    * other syntactic_continuations immediately.
    *
    *======================================================================== */
    , public configurator<syntactic_continuation>
  {
    std::unordered_map<std::string, object> symbols;
    std::unordered_map<std::string, object> external_symbols;

    std::unordered_map<
      object, // identifier
      object  // value
    > changes;

  public: // Constructors
    template <typename... Ts>
    explicit syntactic_continuation(Ts&&... operands)
      : pair {std::forward<decltype(operands)>(operands)...}
    {
      boot(layer<0>);
    }

    template <auto N>
    explicit syntactic_continuation(std::integral_constant<decltype(N), N>);

  public: // Interfaces
    template <auto N>
    auto boot(std::integral_constant<decltype(N), N>);

    // TODO Rename to "interaction_ready"
    auto ready() const noexcept
    {
      return reader<syntactic_continuation>::ready();
    }

    const auto& intern(const std::string& s)
    {
      if (auto iter {symbols.find(s)}; iter != std::end(symbols))
      {
        return (*iter).second;
      }
      else
      {
        const auto [position, success] {
          symbols.emplace(
            s,
            make<symbol>(s))
        };

        assert(success);
        return (*position).second;
      }
    }

    template <typename... Ts>
    const auto&
      change(
        const object& identifier,
        Ts&&... operands)
    {
      changes.erase(identifier);

      const auto [position, success] {
        changes.emplace(
          identifier,
          std::forward<decltype(operands)>(operands)...)
      };

      assert(success);
      return (*position).second;
    }

    const auto&
      stage(
        const object& identifier)
    {
      return identifier;
    }

    template <typename T, typename... Ts>
    decltype(auto) define(const std::string& name, Ts&&... operands)
    {
      return
        machine<syntactic_continuation>::define(
          intern(name),
          make<T>(
            name,
            std::forward<decltype(operands)>(operands)...));
    }

    template <typename... Ts>
    decltype(auto) define(const std::string& name, Ts&&... operands)
    {
      return
        machine<syntactic_continuation>::define(
          intern(name),
          std::forward<decltype(operands)>(operands)...);
    }

    std::size_t generation {0};

    const auto& rename(const object& object)
    {
      if (not object.is<symbol>())
      {
        if (verbose == true_object or verbose_environment == true_object)
        {
          std::cerr << "; package\t; renamer ignored non-symbol object "
                    << object
                    << std::endl;
        }

        return object;
      }
      else
      {
        // XXX TIME STAMP REQUIRED???
        // const std::string name {
        //   object.as<const std::string>() + "." + std::to_string(generation)
        // };

        if (verbose == true_object or verbose_environment == true_object)
        {
          // std::cerr << "; auto-rename\t; renaming " << object << " => " << name << std::endl;
          std::cerr << "; package\t; renaming " << object << std::endl;
        }

        return intern(object.as<symbol>());
        // return intern(name);
      }
    }

    decltype(auto) continuation()
    {
      return std::get<0>(*this);
    }

    decltype(auto) current_expression()
    {
      return car(continuation());

      // if (auto& k {continuation()}; not k)
      // {
      //   return k;
      // }
      // else
      // {
      //   return car(k);
      // }
    }

    decltype(auto) lexical_environment()
    {
      return cdr(continuation());

      // if (auto& k {continuation()}; not k)
      // {
      //   return k;
      // }
      // else
      // {
      //   return cdr(k);
      // }
    }

    // history
    decltype(auto) interaction_environment() noexcept
    {
      return std::get<1>(*this);
    }

    decltype(auto) expand(const object& operands)
    {
      // std::cerr << "; macroexpand\t; " << operands << std::endl;

      ++generation;

      push(
        d,
        s,
        e,
        cons(make<instruction>(mnemonic::STOP), c));

      s = unit;

      e = cons(operands, lexical_environment());

      c = current_expression();

      // std::cerr << ";\t\t; s = " << s << std::endl;
      // std::cerr << ";\t\t; e = " << e << std::endl;
      // std::cerr << ";\t\t; c = " << c << std::endl;
      // std::cerr << ";\t\t; d = " << d << std::endl;

      const auto result {execute()};
      // std::cerr << "; \t\t; " << result << std::endl;
      return result;
    }

    // TODO CONVERT TO VM INSTRUCTION
    template <typename... Ts>
    decltype(auto) evaluate(Ts&&... operands)
    {
      return
        execute_interrupt(
          compile(
            std::forward<decltype(operands)>(operands)...));
    }

    template <typename... Ts>
    decltype(auto) load(Ts&&... operands)
    {
      const std::string path {std::forward<decltype(operands)>(operands)...};

      if (verbose == true_object or verbose_loader == true_object)
      {
        std::cerr << "; loader\t; open \"" << path << "\" => ";
      }

      if (std::fstream stream {path}; stream)
      {
        if (verbose == true_object or verbose_loader == true_object)
        {
          std::cerr << "succeeded" << std::endl;
        }

        push(d, s, e, c);
        s = e = c = unit;

        for (auto e {read(stream)}; e != characters.at("end-of-file"); e = read(stream))
        {
          if (verbose == true_object or verbose_reader == true_object)
          {
            std::cerr << "; read\t\t; " << e << std::endl;
          }

          evaluate(e);
        }

        s = pop(d);
        e = pop(d);
        c = pop(d);

        return unspecified;
      }
      else
      {
        if (verbose == true_object or verbose_loader == true_object)
        {
          std::cerr << "failed" << std::endl;
        }

        throw evaluation_error {"failed to open file ", std::quoted(path)};
      }
    }
  };

  #define DEFINE_SPECIAL(NAME, RULE)                                           \
  define<special>(NAME, [this](auto&&... operands)                             \
  {                                                                            \
    return                                                                     \
      RULE(                                                                    \
        std::forward<decltype(operands)>(operands)...);                        \
  })

  #define DEFINE_PROCEDURE_X(NAME, CALLEE)                                     \
  define<procedure>(NAME, [this](auto&&, auto&& operands)                      \
  {                                                                            \
    return                                                                     \
      CALLEE(                                                                  \
        car(operands));                                                        \
  })

  #define DEFINE_PROCEDURE_S(NAME, CALLEE)                                     \
  define<procedure>(NAME, [this](auto&&, auto&& operands)                      \
  {                                                                            \
    return                                                                     \
      CALLEE(                                                                  \
        car(operands).template as<const string>());                            \
  })

  template <>
  auto
    syntactic_continuation::boot(
      std::integral_constant<decltype(0), 0>)
  {
    // DEFINE_PROCEDURE_X("compile",  compile);
    DEFINE_PROCEDURE_X("evaluate", evaluate);

    define<special>("export", [this](
      auto&& expression,
      auto&&,
      auto&& continuation,
      auto&&)
    {
      std::cerr << "; export\t; export-set = " << expression << std::endl;

      for (const auto& each : expression)
      {
        std::cerr << ";\t\t; staging " << each << std::endl;
        external_symbols.emplace(
          write(std::stringstream {}, each).str(),
          each);
      }

      const auto identifiers {
        std::accumulate(
          std::begin(external_symbols), std::end(external_symbols),
          unit,
          [](auto&& value, auto&& pare)
          {
            return cons(pare.second, value);
          })
      };

      return
        cons(
          make<instruction>(mnemonic::LOAD_LITERAL), identifiers, // TODO Change to current-syntactic-continuation (with std::make_shared_from_this)
          continuation);
    });
  }

  template <>
  auto
    syntactic_continuation::boot(
      std::integral_constant<decltype(1), 1>)
  {
    DEFINE_SPECIAL("begin",     sequence);
    DEFINE_SPECIAL("define",    definition);
    DEFINE_SPECIAL("if",        conditional);
    DEFINE_SPECIAL("lambda",    lambda);
    DEFINE_SPECIAL("quote",     quotation);
    DEFINE_SPECIAL("reference", reference);
    DEFINE_SPECIAL("set!",      assignment);

    // DEFINE_SPECIAL("fork-with-current-sytanctic-continuation", fork);
    DEFINE_SPECIAL("call-with-current-continuation",           call_cc);
    DEFINE_SPECIAL("call-with-current-syntactic-continuation", call_csc);

    DEFINE_PROCEDURE_S("load",   load);
    DEFINE_PROCEDURE_S("linker", make<meevax::posix::linker>);

    define<procedure>("procedure-from", [this](auto&&, auto&& operands)
    {
      const std::string name {cadr(operands).template as<string>()};

      return
        make<procedure>(
          name,
          car(operands)
            .template as<posix::linker>()
              .template link<procedure::signature>(name));
    });

    define<procedure>("read", [this](auto&&, auto&& operands)
    {
      return
        read(
          operands ? car(operands).template as<input_file>() : std::cin);
    });

    define<procedure>("write", [this](auto&&, auto&& operands)
    {
      std::cout << car(operands);
      return unspecified;
    });
  }

  template <>
  auto
    syntactic_continuation::boot(
      std::integral_constant<decltype(2), 2>)
  {
    static const std::string layer_1 {
      &_binary_layer_1_ss_start, &_binary_layer_1_ss_end
    };

    std::stringstream stream {layer_1};

    std::size_t counts {0};

    for (auto e {read(stream)}; e != characters.at("end-of-file"); e = read(stream))
    {
      std::cerr << "; layer-1\t; "
                << counts++
                << " expression loaded"
                << std::endl;

      evaluate(e);

      static constexpr auto cursor_up {"\x1b[1A"};

      std::cerr << cursor_up << "\r\x1b[K" << std::flush;
    }

    std::cerr << std::endl;
  }

  #undef DEFINE_SPECIAL
  #undef DEFINE_PROCEDURE_X
  #undef DEFINE_PROCEDURE_S

  template <>
  syntactic_continuation::syntactic_continuation(
    std::integral_constant<decltype(1), 1>)
    : syntactic_continuation::syntactic_continuation {}
  {
    boot(layer<1>);
  }

  template <auto N>
  syntactic_continuation::syntactic_continuation(
    std::integral_constant<decltype(N), N>)
    : syntactic_continuation::syntactic_continuation {layer<N - 1>}
  {
    boot(layer<N>);
  }

  std::ostream& operator<<(std::ostream& os, const syntactic_continuation& syntactic_continuation)
  {
    return os << highlight::syntax << "#("
              << highlight::constructor << "syntactic-continuation"
              << attribute::normal << highlight::comment << " #;" << &syntactic_continuation << attribute::normal
              << highlight::syntax << ")"
              << attribute::normal;
  }
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CONTINUATION_HPP

