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
  struct module
    : public std::unordered_map<std::string, objective> // The symbol table.
    , public reader<module>
  {
    const objective name, declaration;

    machine execute;

    // Default constructor provides "pure" execution context.
    // Pure execution context contains minimal Scheme procedures to bootstrap
    // any other standard Scheme procedures.
    // This constructor typically called only once from main function.
    module();

    module(const objective& name, const objective& declaration = unit)
      : name {name}
      , declaration {declaration}
    {
      std::cerr << "[debug] preparing library:\n"
                << "        name: " << name << "\n"
                << "        declaration: " << declaration << std::endl;
    }

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

    // template <typename... Ts>
    // [[deprecated]] decltype(auto) compile(Ts&&... args) // XXX こんなものを提供しなきゃいけないのがそもそもおかしい
    // {
    //   return execute.compile(std::forward<Ts>(args)...);
    // }

    // template <typename... Ts>
    // decltype(auto) begin(Ts&&... args) // XXX こんなものを提供しなきゃいけないのがそもそもおかしい
    // {
    //   return execute.begin(std::forward<Ts>(args)...);
    // }

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

  public:
    template <typename... Ts>
    decltype(auto) load(Ts&&... args) noexcept(false)
    {
      if (module loader {unit, unit}; loader.open(std::forward<Ts>(args)...), loader.ready())
      {
        loader.merge(*this);
        loader.execute.env = execute.env;
        loader.execute.index.merge(execute.index);

        while (loader.ready())
        {
          const auto expression {loader.read()};
          const auto executable {loader.execute.compile(expression)};
          const auto evaluation {loader.execute(executable)};
        }

        std::cerr << "[debug] " << std::distance(loader.execute.env, execute.env) << " expression defined" << std::endl;

        // TODO ここで export 指定の識別子以外をインデックスから削除
        merge(loader);
        execute.env = loader.execute.env;
        execute.index.merge(loader.execute.index);

        return true_v;
      }
      else
      {
        std::cerr << "[debug] failed to open file" << std::endl; // TODO CONVERT TO EXCEPTION
        return false_v;
      }
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


  module::module()
    : name {unit} // 文字列を受け取って、stringstream 経由でS式へ変換すること
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
  } // module class default constructor

  std::ostream& operator<<(std::ostream&, const module&);
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_MODULE_HPP

