#ifndef INCLUDED_MEEVAX_SYSTEM_MODULE_HPP
#define INCLUDED_MEEVAX_SYSTEM_MODULE_HPP

#include <unordered_map>

#include <meevax/system/reader.hpp>
#include <meevax/system/machine.hpp>

namespace meevax::system
{
  struct module
    : public std::unordered_map<std::string, cursor> // XXX public?
  {
    const std::string name;

    reader file;
    machine secd;

    template <typename... Ts>
    module(const std::string& name, Ts&&... args)
      : std::unordered_map<std::string, cursor> {std::forward<Ts>(args)...}
      , name {name}
      , file {}
      , secd {}
    {}

  public: // reader interface
    bool readable() const noexcept
    {
      return static_cast<bool>(file);
    }

    template <typename... Ts>
    decltype(auto) open(Ts&&... args)
    {
      file = reader {std::forward<Ts>(args)...};
      return readable();
    }

    decltype(auto) read()
    {
      return file.read([&](auto&&... args) { return intern(std::forward<decltype(args)>(args)...); });
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

  private: // symbol table interface
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
  };

  std::ostream& operator<<(std::ostream& os, const module& module)
  {
    return os << "#<module " << module.name << ">";
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_MODULE_HPP

