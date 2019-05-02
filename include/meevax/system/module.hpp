#ifndef INCLUDED_MEEVAX_SYSTEM_MODULE_HPP
#define INCLUDED_MEEVAX_SYSTEM_MODULE_HPP

#include <unordered_map>

#include <meevax/posix/linker.hpp>
#include <meevax/system/machine.hpp>
#include <meevax/system/reader.hpp>

namespace meevax::system
{
  struct module
    : public std::unordered_map<std::string, cursor> // XXX public?
  {
    const std::string name;

    reader source;
    machine secd;

    template <typename... Ts>
    module(const std::string& name, Ts&&... args)
      : std::unordered_map<std::string, cursor> {std::forward<Ts>(args)...}
      , name {name}
    {
      std::cerr << "constructing module \"" << name << "\" => ";
      std::cerr << "done." << std::endl;
    }

  public: // reader interface
    bool readable() const noexcept
    {
      return static_cast<bool>(source);
    }

    template <typename... Ts>
    decltype(auto) open(Ts&&... args)
    {
      source = reader {std::forward<Ts>(args)...};
      return readable();
    }

    decltype(auto) read()
    {
      return source.read([&](auto&&... args) { return intern(std::forward<decltype(args)>(args)...); });
    }

  public: // virtual machine interface
    template <typename T, typename... Ts>
    decltype(auto) define(const std::string& name, Ts&&... args)
    {
      return secd.define(intern(name), make<T>(name, std::forward<Ts>(args)...));
    }

    template <typename... Ts>
    decltype(auto) compile(Ts&&... args)
    {
      return secd.compile(std::forward<Ts>(args)...);
    }

    template <typename... Ts>
    decltype(auto) begin(Ts&&... args)
    {
      return secd.begin(std::forward<Ts>(args)...);
    }

    template <typename... Ts>
    decltype(auto) execute(Ts&&... args)
    {
      return secd.execute(std::forward<Ts>(args)...);
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
    bool load(Ts&&... args) noexcept(false)
    {
      if (reader file {std::forward<Ts>(args)...}; file)
      {
        std::cerr << "[debug] succeeded to open file" << std::endl;

        for (std::size_t size {0}; file; ++size)
        {
          auto expression {file.read([&](auto&&... args) { return intern(std::forward<decltype(args)>(args)...); })};
          execute(compile(expression));

          std::cerr << "\r[debug] " << size << " expression loaded" << std::flush;
        }

        std::cerr << std::endl;

        return true;
      }
      else
      {
        std::cerr << "[debug] failed to open file" << std::endl;
      }

      return false;
    }
  };

  std::ostream& operator<<(std::ostream& os, const module& module)
  {
    return os << "#<module " << module.name << ">";
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_MODULE_HPP

