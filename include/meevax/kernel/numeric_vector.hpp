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

namespace meevax
{
inline namespace kernel
{
  template <typename T>
  using widen = std::conditional_t<std::is_arithmetic_v<T>,
                                   std::conditional_t<std::is_integral_v<T>,
                                                      std::conditional_t<std::is_signed_v<T>, std::intmax_t, std::uintmax_t>,
                                                      double>,
                                   void>;

  template <typename T>
  struct numeric_vector
  {
    std::valarray<T> values;

    explicit numeric_vector() = default;

    explicit numeric_vector(object const& xs)
      : values {}
    {
      values.resize(length(xs));

      std::generate(std::begin(values), std::end(values), [xs = xs]() mutable
      {
        let const x = car(xs);
        xs = cdr(xs);
        return static_cast<widen<T>>(x.as<exact_integer>());
      });
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
      output << std::exchange(whitespace, " ") << cyan(static_cast<widen<T>>(value));
    }

    return output << magenta(")");
  }

  using s8vector = numeric_vector<std::int8_t>;

  using u8vector = numeric_vector<std::uint8_t>;

  using s16vector = numeric_vector<std::int16_t>;

  using u16vector = numeric_vector<std::int16_t>;

  using s32vector = numeric_vector<std::int32_t>;

  using u32vector = numeric_vector<std::int32_t>;

  using s64vector = numeric_vector<std::int64_t>;

  using u64vector = numeric_vector<std::int64_t>;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_NUMERIC_VECTOR_HPP
