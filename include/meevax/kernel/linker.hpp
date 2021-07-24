#ifndef INCLUDED_MEEVAX_KERNEL_LINKER_HPP
#define INCLUDED_MEEVAX_KERNEL_LINKER_HPP

#include <dlfcn.h> // dlopen, dlclose, dlerror

#include <meevax/kernel/error.hpp>

namespace meevax
{
inline namespace kernel
{
  /* ---------------------------------------------------------------------------
   *
   * A toy dynamic-linker provides interface for "dlopen" and "dlsym".
   *
   * ------------------------------------------------------------------------ */
  class [[deprecated]] linker
  {
    struct close
    {
      std::string const name;

      void operator()(void* handle) noexcept
      {
        if (handle && dlclose(handle))
        {
          std::cerr << "failed to close shared library " << name << ": " << dlerror() << std::endl;
        }
      }
    };

    std::string name;

    std::unique_ptr<void, close> handle;

  public:
    auto open(std::string const& name = "")
    {
      dlerror(); // clear

      std::unique_ptr<void, close> result {
        dlopen(name.empty() ? nullptr : name.c_str(), RTLD_LAZY),
        close {name}
      };

      if (auto* message { dlerror() }; message)
      {
        throw file_error(make<string>(message), unit);
      }
      else
      {
        (*this).name = name;
      }

      return result;
    }

    linker(std::string const& name = "")
      : name {name}
      , handle {open(name)}
    {}

    operator bool() const noexcept
    {
      return static_cast<bool>(handle);
    }

    template <typename Signature>
    Signature link(std::string const& symbol) const
    {
      if (handle)
      {
        dlerror(); // clear

        if (void* function {dlsym(handle.get(), symbol.c_str())}; function)
        {
          return reinterpret_cast<Signature>(function);
        }
        else if (auto* message { dlerror() }; message)
        {
          throw error(
            make<string>(string_append("failed to link symbol ", symbol, " of shared library ", name, ": ", message)),
            unit);
        }
        else
        {
          throw error(
            make<string>("failed to link symbol in unexpected situation"),
            unit);
        }
      }
      else
      {
        throw error(
          make<string>("shared library is not opened"),
          unit);
      }
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_LINKER_HPP
