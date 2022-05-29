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

#ifndef INCLUDED_MEEVAX_MEMORY_TAGGED_POINTER_HPP
#define INCLUDED_MEEVAX_MEMORY_TAGGED_POINTER_HPP

#include <cstring>

#include <meevax/memory/simple_pointer.hpp>

namespace meevax
{
inline namespace memory
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
            *reinterpret_cast<std::uintptr_t const*>(&value) << 32 | TAG) }    \
    {}                                                                         \
                                                                               \
    auto operator =(T_##TAG const& value) -> auto &                            \
    {                                                                          \
      simple_pointer<T>::data                                                  \
        = reinterpret_cast<pointer>(                                           \
            *reinterpret_cast<std::uintptr_t const*>(&value) << 32 | TAG);     \
      return *this;                                                            \
    }                                                                          \
                                                                               \
    static_assert(sizeof(T_##TAG) <= 8)

    DEFINE(0b001);
    DEFINE(0b010);
    DEFINE(0b011);

    #undef DEFINE

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
      const auto from = reinterpret_cast<std::uintptr_t>(simple_pointer<T>::data) >> 32;
      U to;
      std::memcpy(&to, &from, sizeof(U));
      return to;
    }

    template <typename U>
    auto is() const noexcept
    {
      return type() == typeid(typename std::decay<U>::type);
    }

    constexpr auto tag() const noexcept
    {
      return reinterpret_cast<std::uintptr_t>(simple_pointer<T>::data) & 0b111;
    }

    constexpr auto type() const noexcept -> decltype(auto)
    {
      switch (tag())
      {
      case 0b000:
        return simple_pointer<T>::operator bool() ? typeid(pointer) : typeid(std::nullptr_t);

      #define DEFINE(TAG)                                                      \
      case TAG:                                                                \
        return typeid(typename std::decay<T_##TAG>::type)

      DEFINE(0b001);
      DEFINE(0b010);
      DEFINE(0b011);

      #undef DEFINE

      default:
        return typeid(void);
      }
    }
  };
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_TAGGED_POINTER_HPP
