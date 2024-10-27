/*
   Copyright 2018-2024 Tatsuya Yamasaki.

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

#include <filesystem>
#include <vector>

#include <meevax/kernel/character.hpp>

namespace meevax
{
inline namespace kernel
{
  struct string : private std::vector<character>
  {
    using std::vector<character>::at;
    using std::vector<character>::back;
    using std::vector<character>::begin;
    using std::vector<character>::emplace_back;
    using std::vector<character>::end;
    using std::vector<character>::insert;
    using std::vector<character>::operator [];
    using std::vector<character>::pop_back;
    using std::vector<character>::push_back;
    using std::vector<character>::rbegin;
    using std::vector<character>::rend;
    using std::vector<character>::reserve;
    using std::vector<character>::size;
    using std::vector<character>::size_type;
    using std::vector<character>::vector;

    explicit string(std::string const&);

    explicit operator std::filesystem::path() const;

    operator std::string() const;

    friend auto operator ==(string const& lhs, string const& rhs) { return static_cast<std::vector<character> const&>(lhs) == static_cast<std::vector<character> const&>(rhs); }
    friend auto operator < (string const& lhs, string const& rhs) { return static_cast<std::vector<character> const&>(lhs) <  static_cast<std::vector<character> const&>(rhs); }
    friend auto operator > (string const& lhs, string const& rhs) { return static_cast<std::vector<character> const&>(lhs) >  static_cast<std::vector<character> const&>(rhs); }
    friend auto operator <=(string const& lhs, string const& rhs) { return static_cast<std::vector<character> const&>(lhs) <= static_cast<std::vector<character> const&>(rhs); }
    friend auto operator >=(string const& lhs, string const& rhs) { return static_cast<std::vector<character> const&>(lhs) >= static_cast<std::vector<character> const&>(rhs); }
  };

  auto operator <<(std::ostream &, string const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_STRING_HPP
