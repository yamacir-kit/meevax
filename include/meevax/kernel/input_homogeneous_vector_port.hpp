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

#ifndef INCLUDED_MEEVAX_KERNEL_INPUT_HOMOGENEOUS_VECTOR_PORT_HPP
#define INCLUDED_MEEVAX_KERNEL_INPUT_HOMOGENEOUS_VECTOR_PORT_HPP

#include <deque>

#include <meevax/kernel/binary_input_port.hpp>
#include <meevax/kernel/eof.hpp>
#include <meevax/kernel/homogeneous_vector.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename T>
  struct input_homogeneous_vector_port : public binary_input_port
  {
    std::deque<T> deque;

    explicit input_homogeneous_vector_port(homogeneous_vector<T> const& v)
      : deque(std::begin(v.valarray), std::end(v.valarray))
    {}

    auto close() -> void override
    {}

    auto get() -> object override
    {
      if (deque.empty())
      {
        return eof_object;
      }
      else
      {
        let const x = make<exact_integer>(deque.front());
        deque.pop_front();
        return x;
      }
    }

    auto get(std::size_t size) -> object override
    {
      if (deque.size() < size)
      {
        return eof_object;
      }
      else
      {
        let const v =  make<homogeneous_vector<T>>(std::begin(deque), std::next(std::begin(deque), size));
        deque.erase(std::begin(deque), std::next(std::begin(deque), size));
        return v;
      }
    }

    auto get_ready() const -> bool override
    {
      return not deque.empty();
    }

    auto is_open() const -> bool override
    {
      return true;
    }

    auto peek() -> object override
    {
      return deque.empty() ? eof_object : make<exact_integer>(deque.front());
    }
  };

  template <typename T>
  auto operator <<(std::ostream & output, input_homogeneous_vector_port<T> const&) -> std::ostream &
  {
    return output << magenta("#,(") << blue("open-input-", homogeneous_vector<T>::tag(), "vector") << magenta(")");
  }

  using input_u8vector_port = input_homogeneous_vector_port<std::uint8_t>;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_INPUT_HOMOGENEOUS_VECTOR_PORT_HPP
