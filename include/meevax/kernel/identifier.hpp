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

#include <meevax/kernel/list.hpp>
#include <meevax/kernel/symbol.hpp>

namespace meevax
{
inline namespace kernel
{
  struct identifier : public virtual pair
  {
    using pair::pair;

    virtual auto is_bound() const -> bool = 0;

    virtual auto is_free() const -> bool = 0;

    auto symbol() const -> const_reference
    {
      return car(*this);
    }
  };

  struct global : public identifier
  {
    using identifier::identifier;

    auto is_bound() const -> bool override;

    auto is_free() const -> bool override;

    // auto symbol() const noexcept -> const_reference;
  };

  auto operator <<(std::ostream &, identifier const&) -> std::ostream &;

  struct local : public virtual pair // de_bruijn_index
  {
    using pair::pair;

    auto is_bound() const -> bool
    {
      return not is_free();
    }

    auto is_free() const -> bool
    {
      return false;
    }
  };

  struct variadic : public local // de_bruijn_index
  {
    using local::local;

    auto is_bound() const -> bool
    {
      return not is_free();
    }

    auto is_free() const -> bool
    {
      return false;
    }
  };

  auto notate(pair::const_reference, pair::const_reference) -> pair::value_type;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_IDENTIFIER_HPP
