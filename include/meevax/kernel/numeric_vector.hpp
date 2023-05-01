/*
   Copyright 2018-2023 Tatsuya Yamasaki.

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

#ifndef INCLUDED_MEEVAX_KERNEL_NUMERIC_VECTOR_HPP
#define INCLUDED_MEEVAX_KERNEL_NUMERIC_VECTOR_HPP

#include <valarray>

#include <meevax/kernel/list.hpp>
#include <meevax/kernel/number.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename T>
  struct numeric_vector
  {
    std::valarray<T> values;

    explicit numeric_vector() = default;

    explicit numeric_vector(object const& xs)
      : values(length(xs))
    {
      std::generate(std::begin(values), std::end(values), [xs = xs]() mutable
      {
        let const x = car(xs);
        xs = cdr(xs);
        return input_cast(x);
      });
    }

    explicit numeric_vector(std::size_t size, object const& x)
      : values(input_cast(x), size)
    {}

    explicit numeric_vector(numeric_vector const& v, std::size_t begin, std::size_t end)
      : values(v.values[std::slice(begin, begin < end ? end - begin : 0, 1)])
    {}

    explicit numeric_vector(numeric_vector const& a, numeric_vector const& b)
      : values(a.values.size() + b.values.size())
    {
      values[std::slice(0, a.values.size(), 1)] = a.values;
      values[std::slice(a.values.size(), b.values.size(), 1)] = b.values;
    }

    explicit numeric_vector(T const* data, std::size_t size)
      : values(data, size)
    {}

    static auto tag() -> auto const&
    {
      auto static const tag = lexical_cast<std::string>(std::is_integral_v<T> ? std::is_signed_v<T> ? 's' : 'u' : 'f', sizeof(T) * CHAR_BIT);
      return tag;
    }

    template <auto I = 0>
    static auto input_cast(object const& x) -> T
    {
      using Us = std::tuple<exact_integer, float, double>;

      if constexpr (I < std::tuple_size_v<Us>)
      {
        using U = std::tuple_element_t<I, Us>;
        return x.is<U>() ? static_cast<T>(x.as<U>()) : input_cast<I + 1>(x);
      }
      else
      {
        throw error(make<string>(lexical_cast<std::string>(tag(), "vector expects real numbers to store, but was given a value that is not")), x);
      }
    }

    static auto output_cast(T x)
    {
      return make<std::conditional_t<std::is_floating_point_v<T>, T, exact_integer>>(x);
    }
  };

  template <typename T>
  auto operator <<(std::ostream & output, numeric_vector<T> const& datum) -> std::ostream &
  {
    static_assert(std::is_arithmetic_v<T>);

    output << magenta("#", std::is_integral_v<T> ? std::is_signed_v<T> ? 's' : 'u' : 'f', sizeof(T) * CHAR_BIT, "(");

    auto whitespace = "";

    for (auto const& value : datum.values)
    {
      output << std::exchange(whitespace, " ") << cyan(numeric_vector<T>::output_cast(value));
    }

    return output << magenta(")");
  }

  template <typename T>
  auto operator ==(numeric_vector<T> const& a, numeric_vector<T> const& b) -> bool
  {
    auto check = [](std::valarray<bool> const& xs)
    {
      return std::all_of(std::begin(xs), std::end(xs), [](auto x) { return x; });
    };

    return check(a.values == b.values);
  }

  using s8vector = numeric_vector<std::int8_t>;

  using u8vector = numeric_vector<std::uint8_t>;

  using s16vector = numeric_vector<std::int16_t>;

  using u16vector = numeric_vector<std::int16_t>;

  using s32vector = numeric_vector<std::int32_t>;

  using u32vector = numeric_vector<std::int32_t>;

  using s64vector = numeric_vector<std::int64_t>;

  using u64vector = numeric_vector<std::int64_t>;

  using f32vector = numeric_vector<float>;

  using f64vector = numeric_vector<double>;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_NUMERIC_VECTOR_HPP
