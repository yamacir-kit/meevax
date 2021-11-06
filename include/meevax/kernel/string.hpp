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

#ifndef INCLUDED_MEEVAX_KERNEL_STRING_HPP
#define INCLUDED_MEEVAX_KERNEL_STRING_HPP

#include <meevax/kernel/character.hpp>

namespace meevax
{
inline namespace kernel
{
  auto cat = [](auto&&... xs)
  {
    std::stringstream ss;
    (ss << ... << xs);
    return ss.str();
  };

  struct string : public std::vector<character>
  {
    using std::vector<character>::vector; // make-string

    explicit string(std::istream &, std::size_t = std::numeric_limits<std::size_t>::max()); // read-string

    explicit string(std::istream &&);

    explicit string(std::string const&);

    template <typename... Ts>
    explicit string(decltype(cat), Ts&&... xs)
      : string { cat(std::forward<decltype(xs)>(xs)...) }
    {}

    auto list(size_type, size_type) const -> object;

    auto list(size_type = 0) const -> object;

    operator std::string() const; // write-string (for display)
  };

  auto operator <<(std::ostream &, string const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_STRING_HPP
