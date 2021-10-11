/*
   Copyright 2018-2021 Tatsuya Yamasaki.

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

#include <iomanip>
#include <ios>

#include <meevax/kernel/port.hpp>
#include <meevax/kernel/reader.hpp>
#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace kernel
{
  #define DEFINE(NAME, STDIO)                                                  \
  standard_##NAME##_port::standard_##NAME##_port()                             \
  {                                                                            \
    copyfmt(STDIO);                                                            \
    clear(STDIO.rdstate());                                                    \
    rdbuf(STDIO.rdbuf());                                                      \
  }                                                                            \
                                                                               \
  auto operator <<(std::ostream & os, standard_##NAME##_port const&) -> std::ostream & \
  {                                                                            \
    return os << magenta << "#,(" << reset << "standard-" #NAME "-port" << magenta << ")" << reset; \
  }                                                                            \
                                                                               \
  let const standard_##NAME = make<standard_##NAME##_port>()

  DEFINE(input,  std:: cin);
  DEFINE(output, std::cout);
  DEFINE(error,  std::cerr);

  #undef DEFINE

  #define DEFINE(TYPENAME, BASE, NAME)                                         \
  TYPENAME::TYPENAME(std::string const& name)                                  \
    : BASE { name }                                                            \
    , name { name }                                                            \
  {}                                                                           \
                                                                               \
  auto operator <<(std::ostream & os, TYPENAME const& datum) -> std::ostream & \
  {                                                                            \
    return os << magenta << "#,(" << green << "open-" NAME " " << datum.name << magenta << ")" << reset; \
  }                                                                            \
  static_assert(true)

  DEFINE(       file_port, std:: fstream,        "file");
  DEFINE( input_file_port, std::ifstream,  "input-file");
  DEFINE(output_file_port, std::ofstream, "outout-file");

  #undef DEFINE

  #define DEFINE(TYPENAME, NAME)                                               \
  auto operator <<(std::ostream & os, TYPENAME const& datum) -> std::ostream & \
  {                                                                            \
    return os << magenta << "#,(" << green << "open-" NAME " " << string(datum.str()) << magenta << ")" << reset; \
  }                                                                            \
  static_assert(true)

  DEFINE(       string_port,        "string");
  DEFINE( input_string_port,  "input-string");
  DEFINE(output_string_port, "output-string");

  #undef DEFINE
} // namespace kernel
} // namespace meevax
