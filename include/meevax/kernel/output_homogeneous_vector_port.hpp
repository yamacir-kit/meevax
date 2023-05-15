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

#ifndef INCLUDED_MEEVAX_KERNEL_OUTPUT_HOMOGENEOUS_VECTOR_PORT_HPP
#define INCLUDED_MEEVAX_KERNEL_OUTPUT_HOMOGENEOUS_VECTOR_PORT_HPP

#include <deque>

#include <meevax/kernel/binary_output_port.hpp>
#include <meevax/kernel/eof.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename T>
  struct output_homogeneous_vector_port : public binary_output_port
  {
    std::deque<T> deque;

    output_homogeneous_vector_port() = default;

    auto flush() -> void override
    {}

    auto put(exact_integer const& x) -> void override
    {
      deque.push_back(static_cast<T>(x));
    }

    auto put(u8vector const& v) -> void override
    {
      std::copy(std::begin(v.values),
                std::end(v.values),
                std::back_inserter(deque));
    }
  };

  template <typename T>
  auto operator <<(std::ostream & output, output_homogeneous_vector_port<T> const&) -> std::ostream &
  {
    return output << magenta("#,(") << blue("open-output-", homogeneous_vector<T>::tag(), "vector") << magenta(")");
  }

  using output_u8vector_port = output_homogeneous_vector_port<std::uint8_t>;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_OUTPUT_HOMOGENEOUS_VECTOR_PORT_HPP
