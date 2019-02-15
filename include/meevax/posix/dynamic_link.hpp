#ifndef INCLUDED_MEEVAX_POSIX_DYNAMIC_LINK_HPP
#define INCLUDED_MEEVAX_POSIX_DYNAMIC_LINK_HPP

#include <iostream>
#include <memory> // std::unique_ptr
#include <sstream>
#include <stdexcept> // std::runtime_error
#include <string>
#include <utility> // std::declval

#include <dlfcn.h> // dlopen, dlclose, dlerror

#include <boost/cstdlib.hpp>

#include <meevax/core/cursor.hpp>
#include <meevax/core/procedure.hpp>

namespace meevax::posix
{
  decltype(auto) link(const std::string& so_name)
  {
    struct deleter
    {
      const std::string so_name;

      void operator()(void* handle) noexcept
      {
        if (dlclose(handle))
        {
          std::cerr << "failed to close " << so_name << " : " << dlerror() << std::endl;
        }
      };
    } close {so_name};

    if (std::unique_ptr<void, deleter> handle {dlopen(so_name.c_str(), RTLD_LAZY), close}; handle)
    {
      return handle;
    }
    else
    {
      std::stringstream buffer {};
      buffer << "failed to link " << so_name << " : " << dlerror();
      throw std::runtime_error {buffer.str()};
    }
  }

  auto load(const decltype(link(std::declval<std::string>()))& link, const std::string& symbol)
  {
    if (void* function {dlsym(link.get(), symbol.c_str())}; function)
    {
      return core::cursor::bind<core::procedure>(symbol, reinterpret_cast<core::procedure::signature>(function));
    }
    else
    {
      std::stringstream buffer {};
      buffer << "failed to load " << link.get_deleter().so_name << " : " << dlerror();
      throw std::runtime_error {buffer.str()};
    }
  }
} // namespace meevax::posix

#endif // INCLUDED_MEEVAX_POSIX_DYNAMIC_LINK_HPP

