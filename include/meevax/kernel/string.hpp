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
  struct string : public std::vector<character>
  {
    explicit string() = default;

    explicit string(std::istream &);

    explicit string(std::string const&);

    explicit string(size_type, character const&);

    template <typename InputIterator>
    explicit string(InputIterator begin, InputIterator end)
      : std::vector<character> { begin, end }
    {}

    // TODO string(std::istream &, size_type k);

    operator std::string() const; // write-string

    auto read(std::istream &) const -> std::vector<character>;

    auto write_string(std::ostream &) const -> std::ostream &;
  };

  auto operator <<(std::ostream &, string const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_STRING_HPP
