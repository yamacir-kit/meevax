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

#ifndef INCLUDED_MEEVAX_TYPE_TRAITS_INTEGER_HPP
#define INCLUDED_MEEVAX_TYPE_TRAITS_INTEGER_HPP

#include <cstdint>
#include <type_traits>

namespace meevax::inline type_traits
{
  template <std::size_t>
  struct int8n;

  template <>
  struct int8n<1>
  {
    using type = std::int8_t;
  };

  template <>
  struct int8n<2>
  {
    using type = std::int16_t;
  };

  template <>
  struct int8n<4>
  {
    using type = std::int32_t;
  };

  template <>
  struct int8n<8>
  {
    using type = std::int64_t;
  };

  template <auto N>
  using int8n_t = typename int8n<N>::type;

  template <std::size_t>
  struct uint8n;

  template <>
  struct uint8n<1>
  {
    using type = std::uint8_t;
  };

  template <>
  struct uint8n<2>
  {
    using type = std::uint16_t;
  };

  template <>
  struct uint8n<4>
  {
    using type = std::uint32_t;
  };

  template <>
  struct uint8n<8>
  {
    using type = std::uint64_t;
  };

  template <auto N>
  using uint8n_t = typename uint8n<N>::type;
} // namespace meevax::type_traits

#endif // INCLUDED_MEEVAX_TYPE_TRAITS_INTEGER_HPP
