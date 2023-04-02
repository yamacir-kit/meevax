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

#include <fstream>

#include <meevax/kernel/string.hpp>

namespace meevax
{
inline namespace kernel
{
  #define DEFINE(NAME, BASE)                                                   \
  struct NAME##_port : public BASE                                             \
  {                                                                            \
    explicit NAME##_port();                                                    \
  };                                                                           \
                                                                               \
  auto operator <<(std::ostream &, NAME##_port const&) -> std::ostream &;      \
                                                                               \
  let extern const NAME

  DEFINE(standard_input,  std::istream);
  DEFINE(standard_output, std::ostream);
  DEFINE(standard_error,  std::ostream);

  #undef DEFINE

  struct file_port : public std::fstream
  {
    string const name;

    template <typename String, typename... Ts>
    explicit file_port(String const& name, Ts&&... xs)
      : std::fstream { name, std::forward<decltype(xs)>(xs)... }
      , name { name }
    {}
  };

  auto operator <<(std::ostream &, file_port const&) -> std::ostream &;

  struct string_port : public std::stringstream
  {
    using std::stringstream::stringstream;
  };

  auto operator <<(std::ostream &, string_port const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PORT_HPP
