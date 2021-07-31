/*
   Copyright 2018-2021 Tatsuya Yamasaki.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

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
