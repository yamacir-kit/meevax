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
#include <meevax/concepts/any_of.hpp>
#include <meevax/kernel/character.hpp>
#include <meevax/kernel/instruction.hpp>
#include <meevax/memory/nan_boxing_pointer.hpp>
#include <meevax/utility/demangle.hpp>

namespace meevax::inline kernel
{
  using null = std::nullptr_t;

  using small_integer = std::int32_t; // Fixed sized integer that can be boxed.
  using widen_integer = std::int64_t; // Fixed sized integer that is temporarily widened to prevent possible overflow.

  struct pair;

  struct object : public nan_boxing_pointer<pair, bool, small_integer, float, character, instruction>
  {
    using pointer = nan_boxing_pointer<pair, bool, small_integer, float, character, instruction>;

    object(std::nullptr_t = nullptr) noexcept;

    object(object const&);

    object(pair *);

    template <any_of<bool, small_integer, float, double, character, instruction> T>
    object(T const& datum)
      : pointer { datum }
    {
      assert(pointer::get() == nullptr);
    }

    ~object();

    auto operator =(object const&) -> object &;

    auto operator =(std::nullptr_t) -> object &;

    auto reset(object const&) -> void;

    auto reset(std::nullptr_t = nullptr) -> void;

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

    auto eqv(object const&) const -> bool;

    template <typename U>
    auto is() const
    {
      return type() == typeid(std::decay_t<U>);
    }

    template <typename U, typename = std::enable_if_t<std::is_class_v<U>>>
    auto is_also() const
    {
      return dynamic_cast<std::add_pointer_t<U>>(pointer::get()) != nullptr;
    }

    auto type() const -> std::type_info const&;

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
      return hash<decltype(x.get())>()(x.get());
    }
  };
}

#endif // INCLUDED_MEEVAX_KERNEL_OBJECT_HPP
