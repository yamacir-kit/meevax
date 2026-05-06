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
  struct homogeneous_vector
  {
    using values_type = std::valarray<T>;

    std::valarray<T> values;

    explicit homogeneous_vector(auto&&... xs)
      : values(std::forward<decltype(xs)>(xs)...)
    {}

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
        throw error(make<string>(tag() + "vector expects real numbers to store, but was given a value that is not"), x);
      }
    }

    static auto output_cast(T x)
    {
      return make<std::conditional_t<std::is_floating_point_v<T>, T, large_integer>>(x);
    }
  };

  template <typename T>
  auto make_homogeneous_vector_from_list_of(object const& xs)
  {
    auto v = make<homogeneous_vector<T>>(length(xs));

    auto i = std::size_t(0);

    for (let const& x : xs)
    {
      v.template as<homogeneous_vector<T>>().values[i++] = homogeneous_vector<T>::input_cast(x);
    }

    return v;
  }

  template <typename T>
  auto operator <<(std::ostream & output, homogeneous_vector<T> const& datum) -> std::ostream &
  {
    static_assert(std::is_arithmetic_v<T>);

    output << magenta("#", homogeneous_vector<T>::tag(), "(");

    auto whitespace = "";

    for (auto value : datum.values)
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

    return check(a.values == b.values);
  }

  #define FOR_EACH_BITS(F) F(8) F(16) F(32) F(64)

  #define DEFINE_EXACT_INTEGER_HOMOGENEOUS_VECTORS(BIT) \
  using s##BIT = std:: int##BIT##_t; \
  using u##BIT = std::uint##BIT##_t; \
  using s##BIT##vector = homogeneous_vector<s##BIT>; \
  using u##BIT##vector = homogeneous_vector<u##BIT>;

  FOR_EACH_BITS(DEFINE_EXACT_INTEGER_HOMOGENEOUS_VECTORS);

  #undef DEFINE_EXACT_INTEGER_HOMOGENEOUS_VECTORS
  #undef FOR_EACH_BITS

  using f32 = float;
  using f64 = double;

  using f32vector = homogeneous_vector<float>;
  using f64vector = homogeneous_vector<double>;
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_HOMOGENEOUS_VECTOR_HPP
