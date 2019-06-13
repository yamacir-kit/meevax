#ifndef INCLUDED_MEEVAX_SYSTEM_ENCLOSURE_HPP
#define INCLUDED_MEEVAX_SYSTEM_ENCLOSURE_HPP

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

  class enclosure
    : public closure // inherits pair type virtually
    , public reader<enclosure>
    , public machine<enclosure>
  {
    std::unordered_map<std::string, object> symbols;

    cursor exported;

  public: // Constructors
    // for syntactic-lambda
    enclosure() = default;

    // for bootstrap scheme-report-environment
    template <int Version>
    enclosure(std::integral_constant<int, Version>);

    // for library constructor
    template <typename... Ts>
    constexpr enclosure(Ts&&... args)
      : pair {std::forward<Ts>(args)...} // virtual base of closure
    {}

  public: // Module System Interface
    auto ready() const noexcept
    {
      return static_cast<bool>(*this); // TODO MORE
    }

    template <typename T, typename... Ts>
    decltype(auto) define(const std::string& name, Ts&&... args)
    {
      return machine<enclosure>::define(intern(name), make<T>(name, std::forward<Ts>(args)...));
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
      return static_cast<cursor&>(std::get<1>(*this));
    }

    decltype(auto) expand(const object& arguments)
    {
      std::cerr << "macroexpand " << arguments << std::endl;

      s = unit;
      e = list(arguments);
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

      const auto master {interaction_environment()};

      if (reader<enclosure> port {path}; port)
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
          interaction_environment() = master;
          std::cerr << "[error] failed to load \"" << path << "\" with no-error; reverted changes for interaction-environment (exclude side-effects)." << std::endl;
          std::swap(*this, port);
          throw;
        }

        std::swap(*this, port);
        std::cerr << "; load  \t; " << std::distance(interaction_environment(), master) << " expression defined" << std::endl;

        s = d.pop();
        e = d.pop();
        c = d.pop();

        return _true_;
      }
      else
      {
        std::cerr << "[debug] failed to open file" << std::endl; // TODO CONVERT TO EXCEPTION
        return _false_;
      }
    }

    template <typename... Ts>
    decltype(auto) import(Ts&&... args)
    {
    }
  };

  template <>
  enclosure::enclosure(std::integral_constant<int, 7>)
  {
    /* 7.1.3
     *
     * <quoation> = '<datum> | (quote <datum>)
     *
     */
    define<special>("quote", [&](auto&& expression, auto&&, auto&& continuation)
    {
      TRACE("compile") << car(expression) << " ; => is <datum>" << std::endl;
      return cons(_ldc_, car(expression), continuation);
    });

    define<special>("car", [&](auto&& exp, auto&& scope, auto&& continuation)
    {
      return compile(
               car(exp),
               scope,
               cons(_car_, continuation)
             );
    });

    define<special>("cdr", [&](auto&& exp, auto&& scope, auto&& continuation)
    {
      return compile(
               car(exp),
               scope,
               cons(_cdr_, continuation)
             );
    });

    define<special>("cons", [&](auto&& exp, auto&& scope, auto&& continuation)
    {
      return compile(
               cadr(exp),
               scope,
               compile(car(exp), scope, cons(_cons_, continuation))
             );
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
               _ldf_,
               body(
                 cdr(expression), // <body>
                 cons(car(expression), lexical_environment), // extend lexical environment
                 list(_return_) // continuation of body (finally, must be return)
               ),
               continuation
             );
    });

    /* 7.1.3
     *
     * (let (<binding-spec>*) <body>)
     *
     * (let <identifier> (<binding-spec>*) <body>)
     *
     */
    // define<special>("let", [&](const object& expression,
    //                            const object& region,
    //                            const object& continuation)
    // {
    //   if (car(expression).is<pair>())
    //   {
    //     return let(expression, region, continuation);
    //   }
    //   else // named-let
    //   {
    //     return continuation; // TODO
    //   }
    // });

    define<special>("macro", [&](auto&& exp, auto&& scope, auto&& continuation)
    {
      TRACE("compile") << car(exp) << " ; => is <formals>" << std::endl;
      return cons(
               _ldm_,
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
  } // enclosure class default constructor

  std::ostream& operator<<(std::ostream& os, const enclosure& enclosure)
  {
    return os << "\x1B[0;36m#<enclosure " << &enclosure << ">\x1b[0m";
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_ENCLOSURE_HPP

