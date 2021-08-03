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

#include <meevax/kernel/port.hpp>
#include <meevax/kernel/reader.hpp>
#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace kernel
{
  let const default_input_port = make<standard_input_port>();

  let const default_output_port = make<standard_output_port>();

  let const default_error_port = make<standard_error_port>();

  void copy_ios(std::ios & from, std::ios & to)
  {
    to.copyfmt(from);
    to.clear(from.rdstate());
    to.rdbuf(from.rdbuf());
  }

  standard_input_port::standard_input_port()
  {
    copy_ios(std::cin, *this);
  }

  auto operator <<(std::ostream & os, standard_input_port const&) -> std::ostream &
  {
    return os << magenta << "#,(" << reset << "standard-input-port" << magenta << ")" << reset;
  }

  standard_output_port::standard_output_port()
  {
    copy_ios(std::cout, *this);
  }

  auto operator <<(std::ostream & os, standard_output_port const&) -> std::ostream &
  {
    return os << magenta << "#,(" << reset << "standard-output-port" << magenta << ")" << reset;
  }

  standard_error_port::standard_error_port()
  {
    copy_ios(std::cerr, *this);
  }

  auto operator <<(std::ostream & os, standard_error_port const&) -> std::ostream &
  {
    return os << magenta << "#,(" << reset << "standard-error-port" << magenta << ")" << reset;
  }

  #define BOILERPLATE(TYPENAME, PORTTYPE)                                      \
  auto operator <<(std::ostream & os, TYPENAME const& datum) -> std::ostream & \
  {                                                                            \
    os << magenta << "#,(" << green << "open-" PORTTYPE << " " << datum.name << reset; \
                                                                               \
    if (not datum.is_open())                                                   \
    {                                                                          \
      os << faint << " #;closed" << reset;                                     \
    }                                                                          \
                                                                               \
    return os << magenta << ")" << reset;                                      \
  } static_assert(true)

  BOILERPLATE( input_file_port,  "input-file");
  BOILERPLATE(output_file_port, "output-file");

  #undef BOILERPLATE

  #define BOILERPLATE(TYPENAME, PORTTYPE)                                      \
  auto operator <<(std::ostream & os, TYPENAME const& datum) -> std::ostream & \
  {                                                                            \
    os << magenta << "#,(" << green << "open-" PORTTYPE;                       \
                                                                               \
    if (auto const s = datum.str(); not std::empty(s))                         \
    {                                                                          \
      os << " " << cyan << make<string>(s);                                    \
    }                                                                          \
                                                                               \
    return os << magenta << ")" << reset;                                      \
  } static_assert(true)

  BOILERPLATE( input_string_port,  "input-string");
  BOILERPLATE(output_string_port, "output-string");

  #undef BOILERPLATE
} // namespace kernel
} // namespace meevax
