#include <memory>

#if __has_include(<cxxabi.h>)
#include <cxxabi.h>
#endif

#include <meevax/utility/demangle.hpp>

namespace meevax
{
inline namespace utility
{
  auto demangle(const pointer<const char> name) -> std::string
  {
  #if __has_include(<cxxabi.h>)
    int failed {};

    std::unique_ptr<char, decltype(&std::free)> demangled
    {
      abi::__cxa_demangle(name, nullptr, nullptr, &failed),
      [](void* x) noexcept -> void { std::free(x); }
    };

    return { failed ? name : demangled.get() };
  #else
    return { name };
  #endif
  }

  auto demangle(std::type_info const& info) -> std::string
  {
    return demangle(info.name());
  }
} // namespace utility
} // namespace meevax
