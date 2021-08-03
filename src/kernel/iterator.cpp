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

#include <meevax/kernel/iterator.hpp>

namespace meevax
{
inline namespace kernel
{
  homoiconic_iterator::homoiconic_iterator(let const& x)
    : std::reference_wrapper<let const> { std::cref(x) }
  {}

  auto homoiconic_iterator::operator *() const -> homoiconic_iterator::const_reference
  {
    return std::get<0>(unwrap().load());
  }

  auto homoiconic_iterator::operator ->() const -> homoiconic_iterator::pointer
  {
    return unwrap();
  }

  auto homoiconic_iterator::operator ++() -> homoiconic_iterator &
  {
    return *this = std::get<1>(unwrap().load());
  }

  auto homoiconic_iterator::operator ++(int) -> homoiconic_iterator
  {
    auto copy = *this;
    operator ++();
    return copy;
  }

  homoiconic_iterator::operator bool() const
  {
    return static_cast<bool>(unwrap());
  }

  auto homoiconic_iterator::unwrap() const noexcept -> const_reference
  {
    return get();
  }

  auto operator ==(homoiconic_iterator const& lhs, homoiconic_iterator const& rhs) noexcept -> bool
  {
    return lhs.get() == rhs.get();
  }

  auto operator !=(homoiconic_iterator const& lhs, homoiconic_iterator const& rhs) noexcept -> bool
  {
    return not (lhs == rhs);
  }
} // namespace kernel
} // namespace meevax

namespace std
{
  auto begin(meevax::let const& x) -> meevax::homoiconic_iterator
  {
    return cbegin(x);
  }

  auto cbegin(meevax::let const& x) -> meevax::homoiconic_iterator
  {
    return x;
  }

  auto cend(meevax::let const&) -> meevax::homoiconic_iterator const&
  {
    static meevax::homoiconic_iterator const cend { meevax::unit };
    return cend;
  }

  auto end(meevax::let const&) -> meevax::homoiconic_iterator const&
  {
    static meevax::homoiconic_iterator const cend { meevax::unit };
    return cend;
  }
} // namespace std
