#ifndef INCLUDED_MEEVAX_UTILITY_DEMANGLE_HPP
#define INCLUDED_MEEVAX_UTILITY_DEMANGLE_HPP

#include <cstdlib>
#include <memory>
#include <string>
#include <typeinfo>

#if __has_include(<cxxabi.h>)
#include <cxxabi.h>
#endif

namespace meevax::utility
{
  auto demangle(const char* name) -> std::string
  {
  #if __has_include(<cxxabi.h>)
    int failed {};

    std::unique_ptr<char, decltype(&std::free)> demangled
    {
      abi::__cxa_demangle(name, nullptr, nullptr, &failed),
      [](void* x) noexcept -> void { std::free(x); }
    };

    return {failed ? name : demangled.get()};
  #else
    return {name};
  #endif
  }

  decltype(auto) demangle(const std::type_info& info)
  {
    return demangle(info.name());
  }
} // namespace meevax::utility

#endif // INCLUDED_MEEVAX_UTILITY_DEMANGLE_HPP

