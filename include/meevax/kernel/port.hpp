/*
   Copyright 2018-2022 Tatsuya Yamasaki.

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

  #define DEFINE(TYPENAME, FILE_STREAM)                                        \
  struct TYPENAME : public FILE_STREAM                                         \
  {                                                                            \
    string const pathname;                                                     \
                                                                               \
    explicit TYPENAME(string const&);                                          \
                                                                               \
    explicit TYPENAME(std::string const&);                                     \
  };                                                                           \
                                                                               \
  auto operator <<(std::ostream &, TYPENAME const&) -> std::ostream &

  DEFINE(       file_port, std:: fstream);
  DEFINE( input_file_port, std::ifstream);
  DEFINE(output_file_port, std::ofstream);

  #undef DEFINE

  #define DEFINE(TYPENAME, BASE)                                               \
  struct TYPENAME : public std::BASE                                           \
  {                                                                            \
    using std::BASE::BASE;                                                     \
  };                                                                           \
                                                                               \
  auto operator <<(std::ostream &, TYPENAME const&) -> std::ostream &

  DEFINE(       string_port,  stringstream);
  DEFINE( input_string_port, istringstream);
  DEFINE(output_string_port, ostringstream);

  #undef DEFINE
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PORT_HPP
