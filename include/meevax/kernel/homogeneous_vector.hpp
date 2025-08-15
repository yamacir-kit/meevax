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

#ifndef INCLUDED_MEEVAX_KERNEL_HOMOGENEOUS_VECTOR_HPP
#define INCLUDED_MEEVAX_KERNEL_HOMOGENEOUS_VECTOR_HPP

#include <valarray>

#include <meevax/kernel/error.hpp>
#include <meevax/kernel/list.hpp>

namespace meevax::inline kernel
{
  template <typename T>
  struct homogeneous_vector : private std::valarray<T>
  {
    using          std::valarray<T>::operator [];
    using          std::valarray<T>::size;
    using          std::valarray<T>::valarray;
    using typename std::valarray<T>::value_type;

    auto valarray()       -> decltype(auto) { return static_cast<std::valarray<T>      &>(*this); }
    auto valarray() const -> decltype(auto) { return static_cast<std::valarray<T> const&>(*this); }

    explicit homogeneous_vector(from_list_tag, let xs)
      : std::valarray<T>(length(xs))
    {
      std::generate(std::begin(*this), std::end(*this), [&]() mutable
      {
        let const x = car(xs);
        xs = cdr(xs);
        return input_cast(x);
      });
    }

    static auto tag() -> auto const&
    {
      auto static const tag = (std::is_integral_v<T> ? std::is_signed_v<T> ? "s" : "u" : "f") + std::to_string(sizeof(T) * CHAR_BIT);
      return tag;
    }

    template <auto I = 0, typename Tuple = std::tuple<small_integer, large_integer, float, double>>
    static auto input_cast(object const& x) -> T
    {
      if constexpr (I < std::tuple_size_v<Tuple>)
      {
        using type_i = std::tuple_element_t<I, Tuple>;

        return x.is<type_i>() ? static_cast<T>(x.as<type_i>()) : input_cast<I + 1>(x);
      }
      else
      {
        throw error(make<string>(lexical_cast<std::string>(tag(), "vector expects real numbers to store, but was given a value that is not")), x);
      }
    }

    static auto output_cast(T x)
    {
      return make<std::conditional_t<std::is_floating_point_v<T>, T, large_integer>>(x);
    }
  };

  template <typename T>
  auto operator <<(std::ostream & output, homogeneous_vector<T> const& datum) -> std::ostream &
  {
    static_assert(std::is_arithmetic_v<T>);

    output << magenta("#", homogeneous_vector<T>::tag(), "(");

    auto whitespace = "";

    for (auto value : datum.valarray())
    {
      output << std::exchange(whitespace, " ") << cyan(homogeneous_vector<T>::output_cast(value));
    }

    return output << magenta(")");
  }

  template <typename T>
  auto operator ==(homogeneous_vector<T> const& a, homogeneous_vector<T> const& b) -> bool
  {
    auto check = [](std::valarray<bool> const& xs)
    {
      return std::all_of(std::begin(xs), std::end(xs), [](auto x) { return x; });
    };

    return check(a.valarray() == b.valarray());
  }

  using s8vector  = homogeneous_vector<std::int8_t>;

  using s16vector = homogeneous_vector<std::int16_t>;

  using s32vector = homogeneous_vector<std::int32_t>;

  using s64vector = homogeneous_vector<std::int64_t>;

  using u8vector  = homogeneous_vector<std::uint8_t>;

  using u16vector = homogeneous_vector<std::uint16_t>;

  using u32vector = homogeneous_vector<std::uint32_t>;

  using u64vector = homogeneous_vector<std::uint64_t>;

  using f32vector = homogeneous_vector<float>;

  using f64vector = homogeneous_vector<double>;
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_HOMOGENEOUS_VECTOR_HPP
