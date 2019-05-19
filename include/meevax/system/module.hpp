#ifndef INCLUDED_MEEVAX_SYSTEM_MODULE_HPP
#define INCLUDED_MEEVAX_SYSTEM_MODULE_HPP

#include <functional> // std::invoke
#include <unordered_map> // std::unoredered_map

#include <meevax/posix/linker.hpp>
#include <meevax/system/machine.hpp>
#include <meevax/system/reader.hpp>

namespace meevax::system
{
  struct module
    : public std::unordered_map<std::string, objective> // The symbol table.
    , public reader<module>
  {
    const objective name, declaration;

    machine execute;

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

    template <typename... Ts>
    decltype(auto) compile(Ts&&... args) // XXX こんなものを提供しなきゃいけないのがそもそもおかしい
    {
      return execute.compile(std::forward<Ts>(args)...);
    }

    template <typename... Ts>
    decltype(auto) begin(Ts&&... args) // XXX こんなものを提供しなきゃいけないのがそもそもおかしい
    {
      return execute.begin(std::forward<Ts>(args)...);
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
          const auto executable {loader.compile(expression)};
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
  };

  std::ostream& operator<<(std::ostream&, const module&);
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_MODULE_HPP

