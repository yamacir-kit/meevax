#ifndef INCLUDED_MEEVAX_SYSTEM_ENVIRONMENT_HPP
#define INCLUDED_MEEVAX_SYSTEM_ENVIRONMENT_HPP

#include <algorithm> // std::equal
#include <functional> // std::invoke
#include <string> // std::string
#include <unordered_map> // std::unoredered_map
#include <type_traits> // std::integral_constant
#include <numeric> // std::accumulate

/**
 * Global configuration generated by CMake before compilation.
 */
#include <meevax/configure.hpp>

#include <meevax/posix/linker.hpp>
#include <meevax/system/machine.hpp>
#include <meevax/system/reader.hpp>
#include <meevax/system/special.hpp>
#include <meevax/utility/debug.hpp>

namespace meevax::system
{
  template <int Version>
  static constexpr std::integral_constant<int, Version> standard_environment {};

  class environment
    /*
     * The environment is a pair of "current expression" and "global environment
     * (simple association list)". It also has the aspect of a meta-closure that
     * closes the global environment when it constructed (this feature is known
     * as syntactic-closure).
     */
    : public closure

    /*
     * Reader access symbol table of this environment (by member function
     * "intern") via static polymorphism. The environment indirectly inherits
     * the non-copyable class std::istream (reader base class), so it cannot be
     * copied.
     */
    , public reader<environment>

    /*
     * Each environment has one virtual machine and compiler.
     */
    , public machine<environment>
  {
    std::unordered_map<std::string, object> symbols;

    /*
     * Global configuration is shared in all of environments running on same
     * program. Thus, any change of configuration member influences any other
     * environments immediately.
     */
    static inline const configure configuration {};

    static inline std::unordered_map<object, posix::linker> linkers {};

  public: // Constructors
    // for macro
    environment() = default;

    // for bootstrap scheme-report-environment
    template <int Version>
    environment(std::integral_constant<int, Version>);

    // for library constructor
    template <typename... Ts>
    constexpr environment(Ts&&... args)
      : pair {std::forward<Ts>(args)...} // virtual base of closure
    {}

  public: // Module System Interface
    auto ready() const noexcept
    {
      return reader<environment>::ready();
    }

    template <typename T, typename... Ts>
    decltype(auto) global_define(const std::string& name, Ts&&... args)
    {
      return system::machine<environment>::define(
               intern(name), make<T>(name, std::forward<Ts>(args)...)
             );
    }

    template <typename... Ts>
    decltype(auto) global_define(const std::string& name, Ts&&... xs)
    {
      return system::machine<environment>::define(
               intern(name), std::forward<Ts>(xs)...
             );
    }

    const auto& intern(const std::string& s)
    {
      if (auto iter {symbols.find(s)}; iter != std::end(symbols))
      {
        return iter->second;
      }
      else
      {
        iter = symbols.emplace(s, make<symbol>(s)).first;
        return iter->second;
      }
    }

    decltype(auto) current_expression() noexcept
    {
      return std::get<0>(*this);
    }

    decltype(auto) interaction_environment() noexcept
    {
      return static_cast<stack&>(std::get<1>(*this));
    }

    decltype(auto) expand(const object& operands)
    {
      std::cerr << "macroexpand " << operands << std::endl;

      s = unit;
      e = list(operands);
      c = current_expression();
      d = cons(
            unit,         // s
            unit,         // e
            list(_stop_), // c
            unit          // d
          );

      return execute();
    }

  public: // LIBRARY SYSTEM INTERFACES
    auto find_library(const object& name)
    {
      for (const object& each : interaction_environment())
      {
        if (const object& key {car(each)}; not key.is<symbol>())
        {
          if (is_same(key, name))
          {
            return cadr(each);
          }
        }
      }

      return unit;
    }

    decltype(auto) import_library()
    {
    }

    decltype(auto) export_library()
    {
    }

  public:
    template <typename... Ts>
    [[deprecated]] decltype(auto) load(Ts&&... args)
    {
      const std::string path {std::forward<Ts>(args)...};

      const auto checkpoint {interaction_environment()};

      if (reader<environment> port {path}; port)
      {
        std::swap(*this, port);

        d.push(s, e, c);
        s = e = c = unit;

        while (ready()) try
        {
          const auto expression {read()};
          // std::cerr << "[loader] expression: " << expression << std::endl;
          const auto executable {compile(expression)};
          // std::cerr << "[loader] executable: " << executable << std::endl;
          const auto evaluation {execute(executable)};
          // std::cerr << "[loader] evaluation: " << evaluation << std::endl;
        }
        catch (...)
        {
          interaction_environment() = checkpoint;
          std::cerr << "[error] failed to load \"" << path << "\" with no-error; reverted changes for interaction-environment (exclude side-effects)." << std::endl;
          std::swap(*this, port);
          throw;
        }

        std::swap(*this, port);
        std::cerr << "; load  \t; " << std::distance(interaction_environment(), checkpoint) << " expression defined" << std::endl;

        s = d.pop();
        e = d.pop();
        c = d.pop();

        return interaction_environment();
      }
      else
      {
        throw error {"failed to open file \"", path, "\""};
      }
    }
  };

  template <>
  environment::environment(std::integral_constant<int, 0>)
  {
    global_define<special>("quote", [&](auto&& expression, auto&&, auto&& continuation, auto)
    {
      TRACE("compile") << car(expression) << " ; => is <datum>" << std::endl;
      return cons(_load_literal_, car(expression), continuation);
    });

    global_define<special>("if", [&](auto&& expression, auto&& lexical_environment, auto&& continuation, auto tail)
    {
      TRACE("compile") << car(expression) << " ; => is <test>" << std::endl;

      if (tail)
      {
        const auto consequent {compile(cadr(expression), lexical_environment, list(_return_), true)};

        const auto alternate {
          cddr(expression) ? compile(caddr(expression), lexical_environment, list(_return_), true)
                           : unspecified
        };

        return compile(
                 car(expression), // <test>
                 lexical_environment,
                 cons(_select_tail_, consequent, alternate, cdr(continuation))
               );
      }
      else
      {
        const auto consequent {compile(cadr(expression), lexical_environment, list(_join_))};

        const auto alternate {
          cddr(expression) ? compile(caddr(expression), lexical_environment, list(_join_))
                           : unspecified
        };

        return compile(
                 car(expression), // <test>
                 lexical_environment,
                 cons(_select_, consequent, alternate, continuation)
               );
      }
    });

    global_define<special>("define", [&](auto&& expression, auto&& region, auto&& continuation, auto)
    {
      if (not region)
      {
        TRACE("compile") << car(expression) << " ; => is <variable>" << std::endl;

        return compile(
                 cdr(expression) ? cadr(expression) : undefined,
                 region,
                 cons(_define_, car(expression), continuation)
               );
      }
      else
      {
        throw error {"syntax error at internal define"};
      }
    });

    global_define<special>("begin", [&](auto&&... args)
    {
      return sequence(std::forward<decltype(args)>(args)...);
    });

    global_define<special>("call-with-current-continuation", [&](auto&& expression, auto&& lexical_environment, auto&& continuation, auto)
    {
      TRACE("compile") << car(expression) << " ; => is <procedure>" << std::endl;

      return cons(
               _make_continuation_,
               continuation,
               compile(
                 car(expression),
                 lexical_environment,
                 cons(_apply_, continuation)
               )
             );
    });

    global_define<special>("lambda", [&](auto&& expression, auto&& lexical_environment, auto&& continuation, auto)
    {
      TRACE("compile") << car(expression) << " ; => is <formals>" << std::endl;

      return cons(
               _make_closure_,
               body(
                 cdr(expression), // <body>
                 cons(car(expression), lexical_environment), // extend lexical environment
                 list(_return_) // continuation of body (finally, must be return)
               ),
               continuation
             );
    });

    global_define<special>("environment", [&](auto&& exp, auto&& scope, auto&& continuation, auto)
    {
      TRACE("compile") << car(exp) << " ; => is <formals>" << std::endl;

      return cons(
               _make_environment_,
               body(
                 cdr(exp),
                 cons(car(exp), scope),
                 list(_return_)
               ),
               continuation
             );
    });

    global_define<special>("set!", [&](auto&&... args)
    {
      return set(std::forward<decltype(args)>(args)...);
    });

    global_define<special>("import", [&](auto&& expression,
                                         auto&& lexical_environment,
                                         auto&& continuation, auto)
    {
      // オペランド評価のための評価器
      environment macro {*this};

      const object library_name {car(expression)};

      if (not library_name)
      {
        throw error {library_name, " is not allowed as library-name"};
      }

      if (const object& library {find_library(library_name)}; library)
      {
        std::cerr << "; import\t; found library " << library_name << " as " << library << std::endl;
      }

      iterator library_path {macro.execute(
        operand(library_name, lexical_environment, continuation, false)
      )};

      std::cerr << "; import\t; library-path\t; " << library_path << std::endl;

      using meevax::system::path;

      object p {std::accumulate(
                  std::begin(library_path),
                  std::end(library_path),
                  make<path>(""),
                  [&](const object& lhs, const object& rhs)
      {
        // std::cerr << ";\t\t; identifier\t; " << rhs << std::endl;

        if (rhs.is<path>())
        {
          return make<path>(lhs.as<path>() /= rhs.as<path>());
        }
        else if (rhs.is<string>())
        {
          return make<path>(lhs.as<path>() /= path {rhs.as<string>()});
        }
        else
        {
          throw error {"unsupported import-set identifier"};
        }
      })};

      std::cerr << ";\t\t; path\t; " << p << std::endl;

      // posix::linker shared_object {library_path.as<path>().c_str()};

      linkers.emplace(library_name, p.as<path>().c_str());

      object library {std::invoke(
        linkers[library_name].link<procedure::signature>("define_library"),
        unit
      )};

      stack executable {cons(
        _load_literal_, library,
        _define_, library_name,
        continuation
      )};

      for (const object& definition : library.as<environment>().interaction_environment())
      {
        std::cerr << "; defition\t; " << definition << std::endl;

        assert(car(definition).is<symbol>());

        executable.push(
          _load_literal_,
          cadr(definition),
          _define_,
          intern(car(definition).as<symbol>())
        );
      }

      return executable;
    });

    global_define<procedure>("load", [&](const object& args)
    {
      return load(car(args).as<const string>());
    });

    global_define<procedure>("symbol", [&](const object& args)
    {
      try
      {
        return make<symbol>(car(args).as<string>());
      }
      catch (...)
      {
        return make<symbol>();
      }
    });

    global_define<procedure>("linker", [&](auto&& args)
    {
      if (auto size {length(args)}; size < 1)
      {
        throw error {"procedure linker expects a string for argument, but received nothing."};
      }
      else if (const object& s {car(args)}; not s.is<string>())
      {
        throw error {
                "procedure linker expects a string for argument, but received ",
                meevax::utility::demangle(s.type()),
                " rest ", size, " argument",
                (size < 2 ? " " : "s "),
                "were ignored."
              };
      }
      else
      {
        return make<meevax::posix::linker>(s.template as<string>());
      }
    });

    global_define<procedure>("link", [&](auto&& args)
    {
      if (auto size {length(args)}; size < 1)
      {
        throw error {"procedure link expects two arguments (linker and string), but received nothing."};
      }
      else if (size < 2)
      {
        throw error {"procedure link expects two arguments (linker and string), but received only one argument."};
      }
      else if (const auto& linker {car(args)}; not linker.template is<meevax::posix::linker>())
      {
        throw error {
                "procedure dynamic-link-open expects a linker for first argument, but received ",
                meevax::utility::demangle(linker.type()),
                " rest ", size - 1, " argument",
                (size < 2 ? " " : "s "),
                "were ignored."
              };
      }
      else if (const auto& name {cadr(args)}; not name.template is<string>())
      {
        throw error {
                "procedure dynamic-link-open expects a string for second argument, but received ",
                meevax::utility::demangle(name.type()),
                " rest ", size - 2, " argument",
                (size < 3 ? " " : "s "),
                "were ignored."
              };
      }
      else
      {
        const auto& linker_ {car(args).template as<meevax::posix::linker>()};
        const std::string& name_ {cadr(args).template as<string>()};
        return make<procedure>(
                 name_,
                 linker_.template link<typename procedure::signature>(name_)
               );
      }
    });

    global_define("version", configuration.version);
    global_define("install-prefix", configuration.install_prefix);

    global_define("standard", make<meevax::system::path>("/home/yamasa/works/meevax/build/lib/meevax"));
    global_define("posix", make<meevax::system::path>("libmeevax-posix.so"));
  } // environment class default constructor

  std::ostream& operator<<(std::ostream& os, const environment& environment)
  {
    return os << "\x1b[35m" << "#("
              << "\x1b[32m" << "environemnt"
              << "\x1b[0m " << "#;" << &environment
              << "\x1b[35m" << ")"
              << "\x1b[0m";
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_ENVIRONMENT_HPP

