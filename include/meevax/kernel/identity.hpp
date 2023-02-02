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
  struct identity : public virtual pair
  {
    using pair::pair;

    virtual auto is_bound() const -> bool = 0;

    virtual auto is_free() const -> bool = 0;

    virtual auto make_load_instruction() const -> object = 0;

    virtual auto make_store_instruction() const -> object = 0;
  };

  struct absolute : public identity
  {
    using identity::identity;

    auto is_bound() const -> bool override;

    auto is_free() const -> bool override;

    auto load() -> object &;

    auto load() const -> object const&;

    auto make_load_instruction() const -> object override;

    auto make_store_instruction() const -> object override;

    auto symbol() const -> object const&;
  };

  auto operator <<(std::ostream &, absolute const&) -> std::ostream &;

  struct keyword : public absolute
  {
    using absolute::absolute;
  };

  struct relative : public identity // (<symbol> . <de Bruijn index>) = (<symbol> <integer> . <integer>)
  {
    using identity::identity;

    auto is_bound() const -> bool override;

    auto is_free() const -> bool override;

    auto load(object const&) -> object &;

    auto load(object const&) const -> object const&;

    auto make_load_instruction() const -> object override;

    auto make_store_instruction() const -> object override;
  };

  auto operator ==(relative const&, relative const&) -> bool;

  auto operator <<(std::ostream &, relative const&) -> std::ostream &;

  struct variadic : public relative // (<symbol> . <de Bruijn index>) = (<symbol> <integer> . <integer>)
  {
    using relative::relative;

    auto load(object const&) -> object &;

    auto load(object const&) const -> object const&;

    auto make_load_instruction() const -> object override;

    auto make_store_instruction() const -> object override;
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_IDENTITY_HPP
