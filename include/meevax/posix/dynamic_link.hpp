#ifndef INCLUDED_MEEVAX_POSIX_DYNAMIC_LINK_HPP
#define INCLUDED_MEEVAX_POSIX_DYNAMIC_LINK_HPP

#include <iostream>
#include <memory> // std::unique_ptr
#include <sstream>
#include <stdexcept> // std::runtime_error
#include <string>

#include <dlfcn.h> // dlopen, dlclose, dlerror

namespace meevax::posix
{
  auto close_dynamic_link = [](void* handle)
  {
    dlclose(handle);
  };

  decltype(auto) dynamic_link(const std::string& name)
  {
    if (std::unique_ptr<void, decltype(close_dynamic_link)> handle {dlopen(name.c_str(), RTLD_LAZY), close_dynamic_link}; handle)
    {
      return handle;
    }
    else
    {
      std::stringstream buffer {};
      buffer << "failed to dynamic link " << name << " : " << dlerror();
      throw std::runtime_error {buffer.str()};
    }
  }
} // namespace meevax::posix

#endif // INCLUDED_MEEVAX_POSIX_DYNAMIC_LINK_HPP

