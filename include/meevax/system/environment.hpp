#ifndef INCLUDED_MEEVAX_SYSTEM_ENVIRONMENT_HPP
#define INCLUDED_MEEVAX_SYSTEM_ENVIRONMENT_HPP

#include <algorithm> // std::equal
#include <functional> // std::invoke
#include <string> // std::string
#include <unordered_map> // std::unoredered_map
#include <type_traits> // std::integral_constant

#include <meevax/posix/linker.hpp>
#include <meevax/system/machine.hpp>
#include <meevax/system/reader.hpp>
#include <meevax/system/special.hpp>
#include <meevax/utility/debug.hpp>

namespace meevax::system
{
  template <int Version>
  static constexpr std::integral_constant<int, Version> scheme_report_environment = {};

  class environment
    : public closure // inherits pair type virtually
    , public reader<environment>
    , public machine<environment>
  {
    std::unordered_map<std::string, object> symbols;

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
    // TODO RENAME TO "char_ready"
    auto ready() const noexcept
    {
      return static_cast<bool>(*this);
    }

    // TODO RENAME TO "global_define"
    template <typename T, typename... Ts>
    decltype(auto) define(const std::string& name, Ts&&... args)
    {
      return machine<environment>::define(intern(name), make<T>(name, std::forward<Ts>(args)...));
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

    decltype(auto) interaction_environment() noexcept
    {
      return static_cast<stack&>(std::get<1>(*this));
    }

    decltype(auto) expand(const object& operands)
    {
      std::cerr << "macroexpand " << operands << std::endl;

      s = unit;
      e = list(operands);
      c = std::get<0>(*this);
      d = cons(
            unit,         // s
            unit,         // e
            list(_stop_), // c
            unit          // d
          );

      return execute();
    }

  public:
    template <typename... Ts>
    decltype(auto) load(Ts&&... args)
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
  environment::environment(std::integral_constant<int, 7>)
  {
    /* 7.1.3
     *
     * <quoation> = '<datum> | (quote <datum>)
     *
     */
    define<special>("quote", [&](auto&& expression, auto&&, auto&& continuation)
    {
      TRACE("compile") << car(expression) << " ; => is <datum>" << std::endl;
      return cons(_load_literal_, car(expression), continuation);
    });

    /* 7.1.3
     *
     * <conditional> = (if <test> <consequent> <alternate>)
     *
     * <test> = <expression>
     * <consequent> = <expression>
     * <alternate> = <expression> | <empty>
     *
     */
    define<special>("if", [&](auto&& expression, auto&& lexical_environment, auto&& continuation)
    {
      TRACE("compile") << car(expression) << " ; => is <test>" << std::endl;
      return compile(
               car(expression), // <test>
               lexical_environment,
               cons(
                 _select_,
                 compile(cadr(expression), lexical_environment, list(_join_)), // <consequent>
                 cddr(expression) ? compile(caddr(expression), lexical_environment, list(_join_)) : unspecified, // <alternate>
                 continuation
               )
             );
    });

    define<special>("define", [&](auto&& expression, auto&& region, auto&& continuation)
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

    /* 7.1.3
     *
     * (begin <sequence>)
     *
     */
    define<special>("begin", [&](auto&&... args)
    {
      return sequence(std::forward<decltype(args)>(args)...);
    });

    define<special>("call-with-current-continuation", [&](auto&& expression, auto&& lexical_environment, auto&& continuation)
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

    /* 7.1.3
     *
     * <lambda expression> = (lambda <formals> <body>)
     *
     * <formals> = (<identifier>*) | (<identifier>+ . <identifier>) | <identifier>
     *
     */
    define<special>("lambda", [&](auto&& expression, auto&& lexical_environment, auto&& continuation)
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

    define<special>("macro", [&](auto&& exp, auto&& scope, auto&& continuation)
    {
      TRACE("compile") << car(exp) << " ; => is <formals>" << std::endl;
      return cons(
               _make_module_,
               body(
                 cdr(exp),
                 cons(car(exp), scope),
                 list(_return_)
               ),
               continuation
             );
    });

    define<special>("set!", [&](auto&&... args)
    {
      return set(std::forward<decltype(args)>(args)...);
    });

    define<procedure>("load", [&](const object& args)
    {
      return load(car(args).as<const string>());
    });

    define<procedure>("make-symbol", [&](const object& args)
    {
      // if (args && car(args) && car(args).type() == typeid(string))
      try
      {
        return make<symbol>(car(args).as<string>());
      }
      // else
      catch (...)
      {
        return make<symbol>();
      }
    });
  } // environment class default constructor

  std::ostream& operator<<(std::ostream& os, const environment& environment)
  {
    return os << "\x1B[0;36m#<environment " << &environment << ">\x1b[0m";
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_ENVIRONMENT_HPP

