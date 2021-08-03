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

#ifndef INCLUDED_MEEVAX_STRING_INDENT_HPP
#define INCLUDED_MEEVAX_STRING_INDENT_HPP

#include <ostream>
#include <string>

namespace meevax
{
  struct indent
  {
    static inline           std::size_t depth         = 0;
    static inline constexpr std::size_t default_width = 2;
    static inline           std::size_t         width = default_width;

    constexpr indent() = default;

    operator std::string() const
    {
      return std::string(depth, ' ');
    }
  };

  auto operator <<(std::ostream & os, indent const& datum) -> std::ostream &;

  auto operator <<(indent &  datum, std::size_t width) -> indent &;
  auto operator <<(indent && datum, std::size_t width) -> indent &;
  auto operator >>(indent &  datum, std::size_t width) -> indent &;
  auto operator >>(indent && datum, std::size_t width) -> indent &;
} // namespace meevax

#endif // INCLUDED_MEEVAX_STRING_INDENT_HPP
