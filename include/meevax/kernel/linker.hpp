#ifndef INCLUDED_MEEVAX_KERNEL_LINKER_HPP
#define INCLUDED_MEEVAX_KERNEL_LINKER_HPP

#include <dlfcn.h> // dlopen, dlclose, dlerror

#include <meevax/kernel/error.hpp>

namespace meevax { inline namespace kernel
{
  /* ===========================================================================
  *
  * A toy dynamic-linker provides interface for "dlopen" and "dlsym".
  *
  *========================================================================== */
  class linker
  {
    /* ==== Close ==============================================================
    *
    * The "dlclose" as custom deleter.
    *
    *======================================================================== */
    struct close
    {
      const std::string name;

      void operator()(void* handle) noexcept
      {
        if (handle && dlclose(handle))
        {
          std::cerr << "failed to close shared library " << name
                    << ": " << dlerror()
                    << std::endl;
          std::exit(boost::exit_failure);
        }
      }
    };

    std::string name;

    std::unique_ptr<void, close> handle;

  public:
    auto open(const std::string& name = "")
    {
      dlerror(); // clear

      std::unique_ptr<void, close> result {
        dlopen(name.empty() ? nullptr : name.c_str(), RTLD_LAZY),
        close {name}
      };

      if (auto* message { dlerror() }; message)
      {
        throw file_error<void>(message);
      }
      else
      {
        (*this).name = name;
      }

      return result;
    }

    linker(const std::string& name = "")
      : name {name}
      , handle {open(name)}
    {}

    operator bool() const noexcept
    {
      return static_cast<bool>(handle);
    }

    template <typename Signature>
    Signature link(const std::string& symbol) const
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
          throw error("failed to link symbol ", symbol, " of shared library ", name, ": ", message);
        }
        else
        {
          throw error("failed to link symbol in unexpected situation");
        }
      }
      else
      {
        throw error("shared library is not opened");
      }
    }
  };
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_LINKER_HPP
