/*
   Copyright 2018-2025 Tatsuya Yamasaki.

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

#ifndef INCLUDED_MEEVAX_KERNEL_OBJECT_HPP
#define INCLUDED_MEEVAX_KERNEL_OBJECT_HPP

#include <cassert>
#include <meevax/kernel/character.hpp>
#include <meevax/kernel/instruction.hpp>
#include <meevax/memory/nan_boxing_pointer.hpp>
#include <meevax/utility/demangle.hpp>

namespace meevax::inline kernel
{
  using null = std::nullptr_t;

  struct pair;

  struct truthy
  {
    auto constexpr operator ()([[maybe_unused]] auto const& x) const noexcept
    {
      assert(x);
      return true;
    }
  };

  struct falsy
  {
    auto constexpr operator ()([[maybe_unused]] auto const& x) const noexcept
    {
      assert(not x);
      return false;
    }
  };

  struct object : public nan_boxing_pointer<pair, bool, small_integer, float, character, instruction>
  {
    using pointer = nan_boxing_pointer<pair, bool, small_integer, float, character, instruction>;

    object(std::nullptr_t = nullptr) noexcept;

    object(object const&) noexcept;

    object(pair *) noexcept;

    template <any_of<bool, small_integer, float, double, character, instruction> T>
    object(T const& datum) noexcept
      : pointer { datum }
    {
      assert(pointer::get() == nullptr);
    }

    ~object() noexcept;

    auto operator =(object const&) noexcept -> object &;

    auto operator =(std::nullptr_t) noexcept -> object &;

    auto erase() const noexcept -> void;

    auto insert() const noexcept -> void;

    template <typename Precondition = std::identity, typename Postcondition = std::identity>
    auto reset(object const& x) noexcept
    {
      if (Precondition()(*this))
      {
        pointer::reset(x);

        if (not Postcondition()(*this))
        {
          erase();
        }
      }
      else
      {
        pointer::reset(x);

        if (Postcondition()(*this))
        {
          insert();
        }
      }
    }

    template <typename From = std::identity>
    auto reset(std::nullptr_t = nullptr) noexcept -> void
    {
      if (From()(*this))
      {
        pointer::reset();
        erase();
      }
      else
      {
        pointer::reset();
      }
    }

    template <typename U>
    auto static as(auto&& x) -> decltype(auto)
    {
      if constexpr (std::is_same_v<std::decay_t<U>, pair>)
      {
        return *x;
      }
      else if constexpr (std::is_class_v<std::decay_t<U>>)
      {
        if (auto data = dynamic_cast<std::add_pointer_t<U>>(x.get()); data)
        {
          return *data;
        }
        else
        {
          throw std::runtime_error("no viable conversion from " + demangle(x.type()) + " to " + demangle(typeid(U)));
        }
      }
      else
      {
        return x.pointer::template as<U>();
      }
    }

    template <typename U> auto as        ()       -> decltype(auto) { return as<U>                     (*this) ; }
    template <typename U> auto as        () const -> decltype(auto) { return as<U>                     (*this) ; }
    template <typename U> auto as_mutable() const -> decltype(auto) { return as<U>(const_cast<object &>(*this)); }

    auto eqv(object const&) const noexcept -> bool;

    auto external_representation() const -> std::string;

    template <typename U>
    auto is() const noexcept
    {
      return type() == typeid(std::decay_t<U>);
    }

    template <typename U, typename = std::enable_if_t<std::is_class_v<U>>>
    auto is_also() const noexcept
    {
      return dynamic_cast<std::add_pointer_t<U>>(pointer::get()) != nullptr;
    }

    auto type() const noexcept -> std::type_info const&;

    auto write(std::ostream &) const -> std::ostream &;
  };

  auto operator <<(std::ostream & os, object const& datum) -> std::ostream &;

  using let = object;

  let extern unit;
} // namespace meevax::kernel

namespace std
{
  template <>
  struct hash<meevax::object>
  {
    auto operator ()(meevax::object const& x) const noexcept
    {
      return hash<decltype(x.data)>()(x.data);
    }
  };
}

#endif // INCLUDED_MEEVAX_KERNEL_OBJECT_HPP
