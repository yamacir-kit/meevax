#ifndef INCLUDED_MEEVAX_POSIX_LINKER_HPP
#define INCLUDED_MEEVAX_POSIX_LINKER_HPP

#include <iostream>
#include <memory> // std::unique_ptr
#include <string>

#include <dlfcn.h> // dlopen, dlclose, dlerror

#include <meevax/utility/demangle.hpp>

namespace meevax::posix
{
  class linker
  {
    struct close
    {
      const std::string path;

      void operator()(void* handle) noexcept
      {
        std::cerr << "closing shared library \"" << path << "\" => ";

        if (handle && dlclose(handle))
        {
          std::cerr << "failed to close shared library, " << dlerror() << std::endl;
        }
        else
        {
          std::cerr << "succeeded" << std::endl;
        }
      };
    };

    std::string path_;

    std::unique_ptr<void, close> handle_;

  public:
    auto open(const std::string& path = "")
      -> std::unique_ptr<void, close>
    {
      std::cerr << "opening shared library \"" << path << "\" => ";

      dlerror(); // clear

      std::unique_ptr<void, close> buffer {
        dlopen(path.empty() ? nullptr : path.c_str(), RTLD_LAZY),
        close {path}
      };

      if (auto* message {dlerror()}; message)
      {
        std::cerr << "failed to open shared library, " << message << std::endl;
        std::exit(EXIT_FAILURE);
      }
      else
      {
        std::cerr << "succeeded." << std::endl;
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
    Signature link(const std::string& name)
    {
      std::cerr << "linking symbol \"" << name << "\" in shared library \"" << path_ << "\" as signature " << utility::demangle(typeid(Signature)) << " => ";

      if (handle_)
      {
        dlerror(); // clear

        if (void* function {dlsym(handle_.get(), name.c_str())}; function)
        {
          std::cerr << "succeeded." << std::endl;

          // XXX Result of this cast is undefined (maybe works file).
          return reinterpret_cast<Signature>(function);
        }
        else if (auto* message {dlerror()}; message)
        {
          std::cerr << "failed to link symbol, " << message << std::endl;
          std::exit(EXIT_FAILURE);
        }
        else
        {
          std::cerr << "failed to link symbol in unexpected situation." << std::endl;
          std::exit(EXIT_FAILURE);
        }
      }
      else
      {
        std::cerr << " shared library is not opened." << std::endl;
      }

      return nullptr;
    }
  };
} // namespace meevax::posix

#endif // INCLUDED_MEEVAX_POSIX_LINKER_HPP

