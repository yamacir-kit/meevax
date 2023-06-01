/*
   Copyright 2018-2023 Tatsuya Yamasaki.

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

#include <meevax/kernel/list.hpp>

namespace meevax
{
inline namespace kernel
{
  iterator::iterator(object const& x)
    : std::reference_wrapper<const object> { std::cref(x) }
  {}

  auto iterator::operator *() const -> iterator::const_reference
  {
    return car(*this);
  }

  auto iterator::operator ->() const -> iterator::pointer
  {
    return &car(*this);
  }

  auto iterator::operator ++() -> iterator &
  {
    return *this = cdr(*this);
  }

  auto iterator::operator ++(int) -> iterator
  {
    auto copy = *this;
    operator ++();
    return copy;
  }

  auto operator ==(iterator const& lhs, iterator const& rhs) noexcept -> bool
  {
    return lhs.get() == rhs.get();
  }

  auto operator !=(iterator const& lhs, iterator const& rhs) noexcept -> bool
  {
    return not (lhs == rhs);
  }
} // namespace kernel
} // namespace meevax

namespace std
{
  auto begin(meevax::object const& x) -> meevax::iterator
  {
    return meevax::iterator(x);
  }

  auto cbegin(meevax::object const& x) -> meevax::iterator
  {
    return meevax::iterator(x);
  }

  auto cend(meevax::object const&) -> meevax::iterator const&
  {
    static auto const cend = meevax::iterator(meevax::unit);
    return cend;
  }

  auto end(meevax::object const&) -> meevax::iterator const&
  {
    static auto const end = meevax::iterator(meevax::unit);
    return end;
  }
} // namespace std
