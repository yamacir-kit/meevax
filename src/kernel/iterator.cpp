/*
   Copyright 2018-2022 Tatsuya Yamasaki.

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
  iterator::iterator(const_reference x)
    : std::reference_wrapper<object const> { std::cref(x) }
  {}

  auto iterator::operator *() const -> iterator::const_reference
  {
    return car(*this);
  }

  auto iterator::operator ->() const -> iterator::pointer
  {
    return unwrap();
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

  iterator::operator bool() const
  {
    return static_cast<bool>(unwrap());
  }

  auto iterator::unwrap() const noexcept -> const_reference
  {
    return get();
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
  auto begin(meevax::const_reference x) -> meevax::iterator
  {
    return cbegin(x);
  }

  auto cbegin(meevax::const_reference x) -> meevax::iterator
  {
    return x;
  }

  auto cend(meevax::const_reference) -> meevax::iterator const&
  {
    static meevax::iterator const cend { meevax::unit };
    return cend;
  }

  auto end(meevax::const_reference) -> meevax::iterator const&
  {
    static meevax::iterator const cend { meevax::unit };
    return cend;
  }
} // namespace std
