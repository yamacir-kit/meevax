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

#ifndef INCLUDED_MEEVAX_KERNEL_PATH_HPP
#define INCLUDED_MEEVAX_KERNEL_PATH_HPP

#ifdef __cpp_lib_filesystem
#include <filesystem>
#define STD_FILESYSTEM_PATH std::filesystem::path
#else
#include <experimental/filesystem>
#define STD_FILESYSTEM_PATH std::experimental::filesystem::path
#endif

namespace meevax
{
inline namespace kernel
{
  struct path : public STD_FILESYSTEM_PATH
  {
    template <typename... Ts>
    explicit constexpr path(Ts&&... xs)
      : STD_FILESYSTEM_PATH { std::forward<decltype(xs)>(xs)... }
    {}
  };

  auto operator <<(std::ostream &, path const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PATH_HPP
