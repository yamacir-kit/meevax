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

#ifndef INCLUDED_MEEVAX_KERNEL_PORT_HPP
#define INCLUDED_MEEVAX_KERNEL_PORT_HPP

#include <meevax/kernel/string.hpp>

namespace meevax
{
inline namespace kernel
{
  struct port
  {};

  struct textual_port : public virtual port
  {};

  struct binary_port : public virtual port
  {};

  struct input_port : public virtual port
  {
    virtual operator std::istream &() = 0;
  };

  struct output_port : public virtual port
  {
    virtual operator std::ostream &() = 0;

    virtual auto flush() -> std::ostream &
    {
      return static_cast<std::ostream &>(*this) << std::flush;
    }
  };

  struct input_textual_port : public virtual input_port
                            , public virtual textual_port
  {
  };

  struct output_textual_port : public virtual output_port
                             , public virtual textual_port
  {
  };

  struct standard_input_port : public input_textual_port
  {
    operator std::istream &() override
    {
      return std::cin;
    }
  };

  auto operator <<(std::ostream &, standard_input_port const&) -> std::ostream &;

  struct standard_output_port : public output_textual_port
  {
    operator std::ostream &() override
    {
      return std::cout;
    }
  };

  auto operator <<(std::ostream &, standard_output_port const&) -> std::ostream &;

  struct standard_error_port : public output_textual_port
  {
    operator std::ostream &() override
    {
      return std::cerr;
    }
  };

  auto operator <<(std::ostream &, standard_error_port const&) -> std::ostream &;

  struct string_port : public input_textual_port
                     , public output_textual_port
  {
    std::stringstream stringstream;

    template <typename... Ts>
    explicit string_port(Ts&&... xs)
      : stringstream { std::forward<decltype(xs)>(xs)... }
    {}

    operator std::istream &() override
    {
      return stringstream;
    }

    operator std::ostream &() override
    {
      return stringstream;
    }
  };

  auto operator <<(std::ostream &, string_port const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PORT_HPP
