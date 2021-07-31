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
  void copy_ios(std::ios & from, std::ios & to)
  {
    to.copyfmt(from);
    to.clear(from.rdstate());
    to.rdbuf(from.rdbuf());
  }

  let const default_input_port  = make<standard_input_port>();
  let const default_output_port = make<standard_output_port>();
  let const default_error_port  = make<standard_error_port>();

  auto operator <<(output_port & port, standard_input_port const&) -> output_port &
  {
    return port << magenta << "#,(" << reset << "standard-input-port" << magenta << ")" << reset;
  }

  auto operator <<(output_port & port, standard_output_port const&) -> output_port &
  {
    return port << magenta << "#,(" << reset << "standard-output-port" << magenta << ")" << reset;
  }

  auto operator <<(output_port & port, standard_error_port const&) -> output_port &
  {
    return port << magenta << "#,(" << reset << "standard-error-port" << magenta << ")" << reset;
  }

  #define BOILERPLATE(TYPENAME, PORTTYPE)                                      \
  auto operator <<(output_port & port, TYPENAME const& datum) -> output_port & \
  {                                                                            \
    port << magenta << "#,(" << green << "open-" PORTTYPE << " " << datum.name << reset; \
                                                                               \
    if (not datum.is_open())                                                   \
    {                                                                          \
      port << faint << " #;closed" << reset;                                   \
    }                                                                          \
                                                                               \
    return port << magenta << ")" << reset;                                    \
  } static_assert(true)

  BOILERPLATE( input_file_port,  "input-file");
  BOILERPLATE(output_file_port, "output-file");

  #undef BOILERPLATE

  #define BOILERPLATE(TYPENAME, PORTTYPE)                                      \
  auto operator <<(output_port & port, TYPENAME const& datum) -> output_port & \
  {                                                                            \
    port << magenta << "#,(" << green << "open-" PORTTYPE;                     \
                                                                               \
    if (auto const s = datum.str(); not std::empty(s))                         \
    {                                                                          \
      port << " " << cyan << make<string>(s);                                  \
    }                                                                          \
                                                                               \
    return port << magenta << ")" << reset;                                    \
  } static_assert(true)

  BOILERPLATE( input_string_port,  "input-string");
  BOILERPLATE(output_string_port, "output-string");

  #undef BOILERPLATE
} // namespace kernel
} // namespace meevax
