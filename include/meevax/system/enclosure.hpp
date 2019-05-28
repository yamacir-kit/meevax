#ifndef INCLUDED_MEEVAX_SYSTEM_ENCLOSURE_HPP
#define INCLUDED_MEEVAX_SYSTEM_ENCLOSURE_HPP

#include <functional> // std::invoke
#include <unordered_map> // std::unoredered_map

#include <meevax/posix/linker.hpp>
#include <meevax/system/machine.hpp>
#include <meevax/system/reader.hpp>
#include <meevax/system/special.hpp>
#include <meevax/utility/debug.hpp>

namespace meevax::system
{
  template <int Version>
  static constexpr std::integral_constant<int, Version> scheme_report_environment = {};

  struct enclosure
    : public closure // inherits pair type virtually
    , public reader<enclosure> // TODO ポートとして独立させること
    , public machine<enclosure>
    , private std::unordered_map<std::string, objective> // namespace
  {
  public: // Constructors
    // for syntactic-lambda
    enclosure() = default;

    // for bootstrap scheme-report-environment
    template <int Version>
    enclosure(std::integral_constant<int, Version>);

    // for load
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
      if (auto iter {find(s)}; iter != std::unordered_map<std::string, objective>::end())
      {
        return iter->second;
      }
      else
      {
        iter = emplace(s, make<symbol>(s)).first;
        return iter->second;
      }
    }

    decltype(auto) interaction_environment() noexcept
    {
      return static_cast<cursor&>(std::get<1>(*this));
    }

    // decltype(auto) expand(const objective& arguments, const objective& expansion_context)
    decltype(auto) expand(const objective& arguments)
    {
      std::cerr << "macroexpand " << arguments << std::endl;

      // interaction_environment() = expansion_context;

      s = unit;
      e = list(arguments);
      d = cons(
            unit,       // s
            unit,       // e
            list(STOP), // c
            unit        // d
          );

      return execute(std::get<0>(*this));
    }

  public:
    // From R7RS 6.14. System Interface
    //
    //   (load <filename>)                               load library procedure
    //
    //   (load <filename> <environment-specifier>)       load library procedure
    //
    // It is an error if filename is not a string.
    //
    // An implementation-dependent operation is used to transform filename into
    // the name of an existing file containing Scheme source code. The load
    // procedure reads expressions and definitions from the file and evaluates
    // them sequentially in the environment specified by <environment-specifier>.
    // If environment-specifier is omitted, (interaction-environment) is assumed.
    //
    // Rationale: For portability, load must operate on source files. Its
    // operation on other kinds of files necessarily varies among
    // implementations.
    //
    template <typename... Ts>
    decltype(auto) load(Ts&&... args)
    {
      if (reader<enclosure> port {std::forward<Ts>(args)...}; port)
      {
        std::swap(*this, port);

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
          std::swap(*this, port);
          throw;
        }

        std::swap(*this, port);

        return true_v;
      }
      else
      {
        std::cerr << "[debug] failed to open file" << std::endl; // TODO CONVERT TO EXCEPTION
        return false_v;
      }

      // if (enclosure loader {unit, interaction_environment()}; loader.open(std::forward<Ts>(args)...), loader.ready())
      // {
      //   loader.merge(*this); // TODO マージだと呼び出し元がシンボルテーブルを手放すことになるため、コピーに変更すること
      //
      //   while (loader.ready()) // 事実上の begin
      //   {
      //     const auto expression {loader.read()};
      //     // std::cerr << "[loader] expression: " << expression << std::endl;
      //     const auto executable {loader.compile(expression)};
      //     // std::cerr << "[loader] executable: " << executable << std::endl;
      //     const auto evaluation {loader.execute(executable)};
      //     // std::cerr << "[loader] evaluation: " << evaluation << std::endl;
      //   }
      //
      //   std::cerr << "[debug] " << std::distance(
      //                                loader.interaction_environment(),
      //                                       interaction_environment())
      //             << " expression defined" << std::endl;
      //
      //   merge(loader);
      //   interaction_environment() = loader.interaction_environment();
      //
      //   return true_v;
      // }
      // else
      // {
      //   std::cerr << "[debug] failed to open file" << std::endl; // TODO CONVERT TO EXCEPTION
      //   return false_v;
      // }
    }

    // From R7RS 5.2. Import Declarations
    //
    // An import declaration takes the following form:
    //
    //   (import <import-set> ...)
    //
    // An import declaration provides a way to import identifiers exported by a
    // library. Each <import-set> names a set of bindings from a library and
    // possibly specifies local names for the imported bindings. It takes one
    // of the following forms:
    //
    //   <library-name>
    //
    //   (only <import-set> <identifier> ...)
    //
    //   (except <import-set> <identifier> ...)
    //
    //   (prefix <import-set> <identifier>)
    //
    //   (rename <import-set> (<identifier_1> <identifier_2> ...)
    //
    // In the first form, all of the identifiers in the named library's export
    // clauses are imported with the same names (or the exported names if
    // exported with rename). The additional <import-set> forms modify this set
    // as follows:
    //
    //   only produces a subset of the given <import-set> including only the
    //   listed identifiers (after any renaming). It is an error if any of the
    //   listed identifiers are not found in the original set.
    //
    //   except produces a subset of the given <import-set>, excluding the
    //   listed identifiers (after any renaming). It is an error if any of the
    //   listed identifiers are not found in the original set.
    //
    //   rename modifies the given <import-set>, replacing each instance of
    //   <identifier_1> with <identifier_2>. It is an error if any of the listed
    //   <identifier_1>s are not found in the original set.
    //
    //   prefix automatically renames all identifiers in the given <import-set>,
    //   prefixing each with the specified identifier.
    //
    // In a program or library declaration, it is an error to import the same
    // identifier more than once with different bindings, or to redefine or
    // mutate an imported binding with a definition or with set!, or to refer to
    // an identifier before it is imported. However, a REPL should permit these
    // actions.
    //
    template <typename... Ts>
    decltype(auto) import(Ts&&... args)
    {
    }
  };

  template <>
  enclosure::enclosure<7>(std::integral_constant<int, 7>)
  {
    define<special>("quote", [&](auto&& expr,
                                 auto&&,
                                 auto&& continuation)
    {
      TRACE("compile") << cadr(expr) << " ; => is quoted data" << std::endl;
      return cons(LDC, cadr(expr), continuation);
    });

    define<special>("car", [&](auto&& exp,
                               auto&& scope,
                               auto&& continuation)
    {
      return compile(
               cadr(exp),
               scope,
               cons(CAR, continuation)
             );
    });

    define<special>("cdr", [&](auto&& exp,
                               auto&& scope,
                               auto&& continuation)
    {
      return compile(
               cadr(exp),
               scope,
               cons(CDR, continuation)
             );
    });

    define<special>("cons", [&](auto&& exp,
                                auto&& scope,
                                auto&& continuation)
    {
      return compile(
               caddr(exp),
               scope,
               compile(cadr(exp), scope, cons(CONS, continuation))
             );
    });

    define<special>("if", [&](auto&& exp,
                              auto&& scope,
                              auto&& continuation)
    {
      TRACE("compile") << cadr(exp) << " ; => is <test>" << std::endl;
      return compile(
               cadr(exp), // conditional expression
               scope,
               cons(
                 SELECT,
                 compile( caddr(exp), scope, list(JOIN)), // consequent expression
                 compile(cadddr(exp), scope, list(JOIN)), // alternate expression
                 continuation
               )
             );
    });

    define<special>("define", [&](auto&& exp,
                                  auto&& scope,
                                  auto&& continuation)
    {
      TRACE("compile") << cadr(exp) << " ; => is <variable>" << std::endl;
      return compile(
               caddr(exp),
               scope,
               cons(DEFINE, cadr(exp), continuation)
             );
    });

    define<special>("lambda", [&](auto&& exp,
                                  auto&& scope,
                                  auto&& continuation)
    {
      TRACE("compile") << cadr(exp) << " ; => is <formals>" << std::endl;
      return cons(
               LDF,
               body(
                 cddr(exp),
                 cons(
                   cadr(exp), // parameters
                   scope
                 ),
                 list(RETURN)
               ),
               continuation
             );
    });

    define<special>("macro", [&](auto&& exp,
                                 auto&& scope,
                                 auto&& continuation)
    {
      TRACE("compile") << cadr(exp) << " ; => is <formals>" << std::endl;
      return cons(
               LDS,
               body(
                 cddr(exp),
                 cons(
                   cadr(exp), // parameters
                   scope
                 ),
                 list(RETURN)
               ),
               continuation
             );
    });

    define<special>("set!", [&](auto&& exp,
                                auto&& scope,
                                auto&& continuation)
    {
      if (!exp)
      {
        throw error {"setting to unit"};
      }
      else if (auto location {locate(cadr(exp), scope)}; location)
      {
        return compile(
                 caddr(exp),
                 scope,
                 cons(SETL, location, continuation)
               );
      }
      else
      {
        return compile(
                 caddr(exp),
                 scope,
                 cons(SETG, cadr(exp), continuation)
               );
      }
    });

    define<procedure>("load", [&](auto&& args)
    {
      // XXX 今は雑にブーリアンを返してる
      return load(car(args).template as<string>());
    });
  } // enclosure class default constructor

  std::ostream& operator<<(std::ostream&, const enclosure&);
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_ENCLOSURE_HPP

