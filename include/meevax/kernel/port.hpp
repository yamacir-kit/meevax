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

#include <meevax/kernel/character.hpp>
#include <meevax/kernel/string.hpp>

namespace meevax
{
inline namespace kernel
{
  struct port
  {
    virtual auto is_open() const -> bool;

    virtual auto close() -> void;
  };

  struct textual_port : public virtual port
  {};

  struct binary_port : public virtual port
  {};

  struct input_port : public virtual port
  {
    virtual auto get_ready() const -> bool = 0;
  };

  struct output_port : public virtual port
  {
    virtual auto flush() -> void = 0;
  };

  struct textual_input_port : public virtual textual_port, public virtual input_port
  {
    auto get() -> object;

    auto get(std::size_t) -> object;

    auto get_ready() const -> bool override;

    auto peek() -> object;

    auto read() -> object;

    explicit virtual operator std::istream &() = 0;

    explicit virtual operator std::istream const&() const = 0;
  };

  struct textual_output_port : public virtual textual_port, public virtual output_port
  {
    auto flush() -> void override;

    auto put(character const&) -> void;

    auto put(string const&) -> void;

    auto write(object const&) -> void;

    auto write_simple(object const&) -> void;

    explicit virtual operator std::ostream &() = 0;
  };

  struct binary_input_port : public virtual binary_port, public virtual input_port
  {
  };

  struct binary_output_port : public virtual binary_port, public virtual output_port
  {
  };

  struct standard_input_port : public textual_input_port
  {
    operator std::istream &() override;

    operator std::istream const&() const override;
  };

  auto operator <<(std::ostream &, standard_input_port const&) -> std::ostream &;

  struct standard_output_port : public textual_output_port
  {
    operator std::ostream &() override;
  };

  auto operator <<(std::ostream &, standard_output_port const&) -> std::ostream &;

  struct standard_error_port : public textual_output_port
  {
    operator std::ostream &() override;
  };

  auto operator <<(std::ostream &, standard_error_port const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PORT_HPP
