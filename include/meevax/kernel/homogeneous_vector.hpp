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

#ifndef INCLUDED_MEEVAX_KERNEL_HOMOGENEOUS_VECTOR_HPP
#define INCLUDED_MEEVAX_KERNEL_HOMOGENEOUS_VECTOR_HPP

#include <valarray>

#include <meevax/kernel/error.hpp>
#include <meevax/kernel/list.hpp>
#include <meevax/kernel/number.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename T>
  struct homogeneous_vector
  {
    std::valarray<T> valarray;

    homogeneous_vector() = default;

    explicit homogeneous_vector(object xs)
      : valarray(length(xs))
    {
      std::generate(std::begin(valarray), std::end(valarray), [&]() mutable
      {
        let const x = car(xs);
        xs = cdr(xs);
        return input_cast(x);
      });
    }

    explicit homogeneous_vector(std::size_t size, object const& x)
      : valarray(input_cast(x), size)
    {}

    explicit homogeneous_vector(homogeneous_vector const& v, std::size_t begin, std::size_t end)
      : valarray(v.valarray[std::slice(begin, begin < end ? end - begin : 0, 1)])
    {}

    explicit homogeneous_vector(homogeneous_vector const& a, homogeneous_vector const& b)
      : valarray(a.valarray.size() + b.valarray.size())
    {
      valarray[std::slice(0, a.valarray.size(), 1)] = a.valarray;
      valarray[std::slice(a.valarray.size(), b.valarray.size(), 1)] = b.valarray;
    }

    explicit homogeneous_vector(T const* data, std::size_t size)
      : valarray(data, size)
    {}

    explicit homogeneous_vector(std::vector<T> const& v)
      : valarray(v.data(), v.size())
    {}

    template <typename Iterator>
    explicit homogeneous_vector(Iterator begin, Iterator end)
      : valarray(std::distance(begin, end))
    {
      std::copy(begin, end, std::begin(valarray));
    }

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
  auto operator <<(std::ostream & output, homogeneous_vector<T> const& datum) -> std::ostream &
  {
    static_assert(std::is_arithmetic_v<T>);

    output << magenta("#", homogeneous_vector<T>::tag(), "(");

    auto whitespace = "";

    for (auto const& value : datum.valarray)
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

    return check(a.valarray == b.valarray);
  }

  using f32vector = homogeneous_vector<float>;

  using f64vector = homogeneous_vector<double>;

  using s8vector  = homogeneous_vector<std::int8_t>;

  using s16vector = homogeneous_vector<std::int16_t>;

  using s32vector = homogeneous_vector<std::int32_t>;

  using s64vector = homogeneous_vector<std::int64_t>;

  using u8vector  = homogeneous_vector<std::uint8_t>;

  using u16vector = homogeneous_vector<std::uint16_t>;

  using u32vector = homogeneous_vector<std::uint32_t>;

  using u64vector = homogeneous_vector<std::uint64_t>;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_HOMOGENEOUS_VECTOR_HPP
