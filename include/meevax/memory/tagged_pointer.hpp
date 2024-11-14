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

#ifndef INCLUDED_MEEVAX_MEMORY_TAGGED_POINTER_HPP
#define INCLUDED_MEEVAX_MEMORY_TAGGED_POINTER_HPP

#include <bit>
#include <stdexcept>

#include <meevax/memory/simple_pointer.hpp>
#include <meevax/type_traits/integer.hpp>

namespace meevax::inline memory
{
  template <typename T,
            typename T_0b001 = std::integral_constant<std::uint32_t, 0b001>,
            typename T_0b010 = std::integral_constant<std::uint32_t, 0b010>,
            typename T_0b011 = std::integral_constant<std::uint32_t, 0b011>>
  struct tagged_pointer : public simple_pointer<T>
  {
    using pointer = typename simple_pointer<T>::pointer;

    using simple_pointer<T>::simple_pointer;

    #define DEFINE(TAG)                                                        \
    explicit constexpr tagged_pointer(T_##TAG const& value)                    \
      : simple_pointer<T> {                                                    \
          reinterpret_cast<pointer>(                                           \
            static_cast<std::uintptr_t>(std::bit_cast<uint8n_t<sizeof(T_##TAG)>>(value)) << 32 | TAG) } \
    {}                                                                         \
                                                                               \
    auto operator =(T_##TAG const& value) -> auto &                            \
    {                                                                          \
      simple_pointer<T>::data                                                  \
        = reinterpret_cast<pointer>(                                           \
            static_cast<std::uintptr_t>(std::bit_cast<uint8n_t<sizeof(T_##TAG)>>(value)) << 32 | TAG); \
      return *this;                                                            \
    }                                                                          \
                                                                               \
    static_assert(sizeof(T_##TAG) <= 4)

    DEFINE(0b001);
    DEFINE(0b010);
    DEFINE(0b011);

    #undef DEFINE

    constexpr auto operator ->() const -> decltype(auto)
    {
      switch (tag())
      {
      case 0b000:
        return simple_pointer<T>::operator ->();

      default:
        throw std::logic_error("");
      }
    }

    constexpr auto operator *() const -> decltype(auto)
    {
      switch (tag())
      {
      case 0b000:
        return simple_pointer<T>::operator *();

      default:
        throw std::logic_error("");
      }
    }

    template <typename U>
    auto as() const
    {
      return std::bit_cast<std::decay_t<U>>(
               static_cast<uint8n_t<sizeof(std::decay_t<U>)>>(
                 reinterpret_cast<std::uintptr_t>(simple_pointer<T>::data) >> 32));
    }

    template <typename U>
    auto is() const noexcept
    {
      return type() == typeid(std::decay_t<U>);
    }

    constexpr auto tag() const noexcept
    {
      return reinterpret_cast<std::uintptr_t>(simple_pointer<T>::data) & 0b111;
    }

    constexpr auto type() const noexcept -> decltype(auto)
    {
      switch (tag())
      {
      #define DEFINE(TAG)                                                      \
      case TAG:                                                                \
        return typeid(std::decay_t<T_##TAG>)

      DEFINE(0b001);
      DEFINE(0b010);
      DEFINE(0b011);

      #undef DEFINE

      case 0b000:
      default:
        return typeid(pointer);
      }
    }
  };
} // namespace meevax::memory

#endif // INCLUDED_MEEVAX_MEMORY_TAGGED_POINTER_HPP
