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

#ifndef INCLUDED_MEEVAX_KERNEL_IDENTIFIER_HPP
#define INCLUDED_MEEVAX_KERNEL_IDENTIFIER_HPP

#include <meevax/kernel/pair.hpp>

namespace meevax
{
inline namespace kernel
{
  struct identifier : public virtual pair
  {
    using pair::pair;

    virtual auto is_bound() const -> bool = 0;

    virtual auto is_free() const -> bool = 0;

    auto symbol() const -> const_reference;
  };

  auto operator <<(std::ostream &, identifier const&) -> std::ostream &;

  struct absolute : public identifier
  {
    using identifier::identifier;

    auto is_bound() const -> bool override;

    auto is_free() const -> bool override;
  };

  struct relative : public identifier // de_bruijn_index
  {
    using identifier::identifier;

    auto is_bound() const -> bool override;

    auto is_free() const -> bool override;
  };

  struct variadic : public relative // de_bruijn_index
  {
    using relative::relative;
  };

  auto notate(pair::const_reference, pair::const_reference) -> object;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_IDENTIFIER_HPP
