#ifndef INCLUDED_MEEVAX_SYSTEM_MODULE_HPP
#define INCLUDED_MEEVAX_SYSTEM_MODULE_HPP

#include <functional> // std::invoke
#include <unordered_map>

#include <meevax/posix/linker.hpp>
#include <meevax/system/machine.hpp>
#include <meevax/system/reader.hpp>

namespace meevax::system
{
  struct module
    : public std::unordered_map<std::string, objective> // XXX public?
  {
    const std::string name;

    reader read_;
    machine execute_; // TODO RENAME

    template <typename... Ts>
    module(const std::string& name, Ts&&... args)
      : std::unordered_map<std::string, objective> {std::forward<Ts>(args)...}
      , name {name}
    {
      std::cerr << "constructing module \"" << name << "\" => ";
      std::cerr << "done." << std::endl;
    }

  public: // reader interface
    auto ready() const noexcept
    {
      return static_cast<bool>(read_); // TODO MORE
    }

    template <typename... Ts>
    decltype(auto) open(Ts&&... args)
    {
      read_ = reader {std::forward<Ts>(args)...};
      return ready();
    }

    decltype(auto) read() // XXX DIRTY WRAPPER
    {
      return std::invoke(read_, [&](auto&&... args) { return intern(std::forward<decltype(args)>(args)...); });
    }

  public: // virtual machine interface
    template <typename T, typename... Ts>
    decltype(auto) define(const std::string& name, Ts&&... args)
    {
      return execute_.define(intern(name), make<T>(name, std::forward<Ts>(args)...));
    }

    template <typename... Ts>
    decltype(auto) compile(Ts&&... args) // XXX こんなものを提供しなきゃいけないのがそもそもおかしい
    {
      return execute_.compile(std::forward<Ts>(args)...);
    }

    template <typename... Ts>
    decltype(auto) begin(Ts&&... args) // XXX こんなものを提供しなきゃいけないのがそもそもおかしい
    {
      return execute_.begin(std::forward<Ts>(args)...);
    }

    template <typename... Ts>
    decltype(auto) execute(Ts&&... args)
    {
      return std::invoke(execute_, std::forward<Ts>(args)...);
    }

  protected: // module interface
    const auto& intern(const std::string& s)
    {
      if (auto iter {find(s)}; iter != std::end(*this))
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
    // XXX TEMPORARY DIRTY HACK
    template <typename... Ts>
    decltype(auto) load(Ts&&... args) noexcept(false)
    {
      if (module loader {"unnamed-loader"}; loader.open(std::forward<Ts>(args)...))
      {
        loader.merge(*this);
        loader.execute_.env = execute_.env;

        while (loader.ready())
        {
          const auto expression {loader.read()};
          std::cerr << "[" << std::size(loader) << "] " << expression << std::flush;

          const auto executable {compile(expression)};
          std::cerr << " => " << executable << std::flush;

          const auto evaluation {execute(executable)};
          std::cerr << " => " << evaluation << std::endl;
        }

        std::cerr << __LINE__ << std::endl;
        merge(loader);
        std::cerr << __LINE__ << std::endl;
        execute_.env = loader.execute_.env;
        std::cerr << __LINE__ << std::endl;
        return true_v;
      }
      else
      {
        std::cerr << "[debug] failed to open file" << std::endl;
        return false_v;
      }
    }
  };

  std::ostream& operator<<(std::ostream&, const module&);
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_MODULE_HPP

