#ifndef INCLUDED_MEEVAX_POSIX_LINKER_HPP
#define INCLUDED_MEEVAX_POSIX_LINKER_HPP

#include <iostream>
#include <memory> // std::unique_ptr
#include <string>

#include <dlfcn.h> // dlopen, dlclose, dlerror

#include <meevax/utility/demangle.hpp>

namespace meevax::posix
{
  inline namespace dirty_hacks
  {
    kernel::object verbose_linker {kernel::false_object};
  }

  #define VERBOSE_LINKER(...)                                                  \
  if (verbose_linker == kernel::true_object)                                   \
  {                                                                            \
    std::cerr << __VA_ARGS__;                                                  \
  }

  /**
   * A toy dynamic-linker provides interface for "dlopen" and "dlsym".
   **/
  class linker
  {
    /**
     * The "dlclose" as custom deleter.
     */
    struct close
    {
      const std::string path;

      void operator()(void* handle) noexcept
      {
        VERBOSE_LINKER("; linker\t; closing shared library \"" << path << "\" => ");

        if (handle && dlclose(handle))
        {
          VERBOSE_LINKER("failed to close shared library " << dlerror() << std::endl);
        }
        else
        {
          VERBOSE_LINKER("succeeded" << std::endl);
        }
      };
    };

    std::string path_;

    std::unique_ptr<void, close> handle_;

  public:
    auto open(const std::string& path = "")
    {
      VERBOSE_LINKER("; linker\t; opening shared library \"" << path << "\" => ");

      dlerror(); // clear

      std::unique_ptr<void, close> buffer {
        dlopen(path.empty() ? nullptr : path.c_str(), RTLD_LAZY),
        close {path}
      };

      if (auto* message {dlerror()}; message)
      {
        VERBOSE_LINKER("failed to open shared library " << message << std::endl);
        std::exit(EXIT_FAILURE);
      }
      else
      {
        VERBOSE_LINKER("succeeded." << std::endl);
        path_ = path;
      }

      return buffer;
    }

    linker(const std::string& path = "")
      : path_ {path}
      , handle_ {open(path)}
    {}

    operator bool() const noexcept
    {
      return static_cast<bool>(handle_);
    }

    template <typename Signature>
    Signature link(const std::string& name) const
    {
      // std::cerr << "; linker\t; linking symbol \"" << name << "\" in shared library \"" << path_ << "\" as signature " << utility::demangle(typeid(Signature)) << " => ";
      VERBOSE_LINKER("; linker\t; linking symbol \"" << name << "\" in shared library \"" << path_ << " => ");

      if (handle_)
      {
        dlerror(); // clear

        if (void* function {dlsym(handle_.get(), name.c_str())}; function)
        {
          VERBOSE_LINKER("succeeded." << std::endl);

          // XXX Result of this cast is undefined (maybe works fine).
          return reinterpret_cast<Signature>(function);
        }
        else if (auto* message {dlerror()}; message)
        {
          VERBOSE_LINKER("failed to link symbol, " << message << std::endl);
          std::exit(EXIT_FAILURE);
        }
        else
        {
          VERBOSE_LINKER("failed to link symbol in unexpected situation." << std::endl);
          std::exit(EXIT_FAILURE);
        }
      }
      else
      {
        VERBOSE_LINKER(" shared library is not opened." << std::endl);
      }

      return nullptr;
    }
  };
} // namespace meevax::posix

#endif // INCLUDED_MEEVAX_POSIX_LINKER_HPP

