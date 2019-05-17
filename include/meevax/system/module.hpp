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
    machine secd; // TODO RENAME

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

        std::size_t size {0};

        while (file)
        {
          auto expression {file.read([&](auto&&... args) { return intern(std::forward<decltype(args)>(args)...); })};
          execute(compile(expression));
          ++size;
        }

        std::cerr << "[debug] " << size << " expression loaded" << std::endl;
        return true;
      }
      else
      {
        std::cerr << "[debug] failed to open file" << std::endl;
      }

      return false;
    }
  };

  std::ostream& operator<<(std::ostream&, const module&);
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_MODULE_HPP

