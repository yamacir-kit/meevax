#ifndef INCLUDED_MEEVAX_UTILITY_DEMANGLE_HPP
#define INCLUDED_MEEVAX_UTILITY_DEMANGLE_HPP

#include <cstdlib>
#include <memory>
#include <string>
#include <typeinfo>

#include <cxxabi.h>

namespace meevax::utility
{
  auto demangle(const char* name)
    -> std::string
  {
    int failed {};

    std::unique_ptr<char, decltype(&std::free)> demangled
    {
      abi::__cxa_demangle(name, nullptr, nullptr, &failed),
      [](auto* x) noexcept { std::free(x); }
    };

    return {failed ? name : demangled.get()};
  }

  decltype(auto) demangle(const std::type_info& info)
  {
    return demangle(info.name());
  }
} // namespace meevax::utility

#endif // INCLUDED_MEEVAX_UTILITY_DEMANGLE_HPP

