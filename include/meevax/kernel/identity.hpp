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

#ifndef INCLUDED_MEEVAX_KERNEL_IDENTITY_HPP
#define INCLUDED_MEEVAX_KERNEL_IDENTITY_HPP

#include <meevax/kernel/pair.hpp>

namespace meevax
{
inline namespace kernel
{
  struct identity
  {
    using index = std::uint32_t;
  };

  struct absolute : public identity
                  , public virtual pair // (<symbol> . <object>)
  {
    using pair::pair;

    auto load() -> object &;

    auto load() const -> object const&;

    auto symbol() const -> object const&;
  };

  auto operator <<(std::ostream &, absolute const&) -> std::ostream &;

  struct keyword : public absolute
  {
    using absolute::absolute;
  };

  struct relative : public identity
                  , public virtual pair // de Bruijn index
  {
    using pair::pair;

    auto load(object const&) -> object &;

    auto load(object const&) const -> object const&;
  };

  struct variadic : public identity
                  , public virtual pair // de Bruijn index
  {
    using pair::pair;

    auto load(object const&) -> object &;

    auto load(object const&) const -> object const&;
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_IDENTITY_HPP
