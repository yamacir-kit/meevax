#ifndef INCLUDED_MEEVAX_SYSTEM_MODULE_HPP
#define INCLUDED_MEEVAX_SYSTEM_MODULE_HPP

#include <functional> // std::invoke
#include <unordered_map> // std::unoredered_map

#include <meevax/posix/linker.hpp>
#include <meevax/system/machine.hpp>
#include <meevax/system/reader.hpp>
#include <meevax/system/special.hpp>

namespace meevax::system
{
  struct syntactic_closure
    : public std::unordered_map<std::string, objective> // namespace
    , public reader<syntactic_closure>
    , public closure
  {
    // const objective declaration;

    objective exported;

    machine<syntactic_closure> execute; // XXX この名前微妙

  public: // Constructors
    // Default constructor provides "pure" execution context. Pure execution
    // context contains minimal Scheme procedures to bootstrap any other
    // standard Scheme procedures. This constructor typically called only once
    // from main function.
    syntactic_closure();

    syntactic_closure(const objective& declaration,
                      const objective& environment_specifier)
      : closure {declaration, environment_specifier}
      , execute {environment_specifier}
    {}

  public: // Reader Interface
    auto ready() const noexcept
    {
      return static_cast<bool>(*this); // TODO MORE
    }

  public: // Virtual Machine Interface
    template <typename T, typename... Ts>
    decltype(auto) define(const std::string& name, Ts&&... args)
    {
      return execute.define(intern(name), make<T>(name, std::forward<Ts>(args)...));
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

    auto& interaction_environment() noexcept
    {
      return execute.env;
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
    decltype(auto) load(Ts&&... args) noexcept(false)
    // decltype(auto) load(const objective& filename,
    //                     const objective& environment_specifier)
    {
      if (syntactic_closure loader {unit, interaction_environment()};
          loader.open(std::forward<Ts>(args)...), loader.ready())
      {
        loader.merge(*this);

        while (loader.ready()) // 事実上の begin
        {
          const auto expression {loader.read()};
          const auto executable {loader.execute.compile(expression)};
          const auto evaluation {loader.execute(executable)};
        }

        std::cerr << "[debug] " << std::distance(
                                     loader.interaction_environment(),
                                            interaction_environment())
                  << " expression defined" << std::endl;

        merge(loader);
        execute.env = loader.interaction_environment();

        return true_v;
      }
      else
      {
        std::cerr << "[debug] failed to open file" << std::endl; // TODO CONVERT TO EXCEPTION
        return false_v;
      }
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

    std::unordered_map<std::string, posix::linker> shared_objects;

    template <typename T, typename... Ts>
    decltype(auto) link(const std::string& path, const std::string& name)
    {
      try
      {
        return shared_objects.at(path).link<typename T::signature>(name);
      }
      catch (const std::out_of_range&)
      {
        shared_objects.emplace(path, path);
        return shared_objects.at(path).link<typename T::signature>(name);
      }
    }
  };

  syntactic_closure::syntactic_closure() // defines interaction-environment
  {
    define<special>("quote", [&](auto&& expr,
                                 auto&&,
                                 auto&& continuation)
    {
      return cons(LDC, cadr(expr), continuation);
    });

    define<special>("car", [&](auto&& exp,
                               auto&& scope,
                               auto&& continuation)
    {
      return execute.compile(
               cadr(exp),
               scope,
               cons(CAR, continuation)
             );
    });

    define<special>("cdr", [&](auto&& exp,
                               auto&& scope,
                               auto&& continuation)
    {
      return execute.compile(
               cadr(exp),
               scope,
               cons(CDR, continuation)
             );
    });

    define<special>("cons", [&](auto&& exp,
                                auto&& scope,
                                auto&& continuation)
    {
      return execute.compile(
               caddr(exp),
               scope,
               execute.compile(cadr(exp), scope, cons(CONS, continuation))
             );
    });

    define<special>("if", [&](auto&& exp,
                              auto&& scope,
                              auto&& continuation)
    {
      return execute.compile(
               cadr(exp), // conditional expression
               scope,
               cons(
                 SELECT,
                 execute.compile( caddr(exp), scope, list(JOIN)), // then expression
                 execute.compile(cadddr(exp), scope, list(JOIN)), // else expression
                 continuation
               )
             );
    });

    define<special>("define", [&](auto&& exp,
                                  auto&& scope,
                                  auto&& continuation)
    {
      return execute.compile(
               caddr(exp),
               scope,
               cons(DEFINE, cadr(exp), continuation)
             );
    });

    define<special>("lambda", [&](auto&& exp,
                                  auto&& scope,
                                  auto&& continuation)
    {
      return cons(
               LDF,
               execute.begin(
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

    define<special>("syntax", [&](auto&& exp,
                                  auto&& scope,
                                  auto&& continuation)
    {
      return cons(
               LDS,
               execute.begin(
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
      else if (auto location {execute.locate(cadr(exp), scope)}; location)
      {
        return execute.compile(
                 caddr(exp),
                 scope,
                 cons(SETL, location, continuation)
               );
      }
      else
      {
        return execute.compile(
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
  } // syntactic_closure class default constructor

  std::ostream& operator<<(std::ostream&, const syntactic_closure&);
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_MODULE_HPP

