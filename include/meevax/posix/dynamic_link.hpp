#ifndef INCLUDED_MEEVAX_POSIX_DYNAMIC_LINK_HPP
#define INCLUDED_MEEVAX_POSIX_DYNAMIC_LINK_HPP

#include <iostream>
#include <memory> // std::unique_ptr
#include <sstream>
#include <stdexcept> // std::runtime_error
#include <string>

#include <dlfcn.h> // dlopen, dlclose, dlerror

#include <boost/cstdlib.hpp>

namespace meevax::posix
{
  decltype(auto) link(const std::string& name)
  {
    auto close = [&](void* handle)
    {
      if (dlclose(handle))
      {
        std::cerr << "failed to close " << name << " : " << dlerror() << std::endl;
      }
    };

    if (std::unique_ptr<void, decltype(close)> handle {dlopen(name.c_str(), RTLD_LAZY), close}; handle)
    {
      return handle;
    }
    else
    {
      std::stringstream buffer {};
      buffer << "failed to link " << name << " : " << dlerror();
      throw std::runtime_error {buffer.str()};
    }
  }
} // namespace meevax::posix

#endif // INCLUDED_MEEVAX_POSIX_DYNAMIC_LINK_HPP

