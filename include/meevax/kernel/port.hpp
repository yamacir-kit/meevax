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

#ifndef INCLUDED_MEEVAX_KERNEL_PORT_HPP
#define INCLUDED_MEEVAX_KERNEL_PORT_HPP

#include <fstream>

#include <meevax/kernel/pair.hpp>
#include <meevax/kernel/path.hpp>

namespace meevax
{
inline namespace kernel
{
  let extern const default_input_port;

  let extern const default_output_port;

  let extern const default_error_port;

  struct standard_input_port : public std::istream
  {
    explicit standard_input_port();
  };

  auto operator <<(std::ostream &, standard_input_port const&) -> std::ostream &;

  struct standard_output_port : public std::ostream
  {
    explicit standard_output_port();
  };

  auto operator <<(std::ostream &, standard_output_port const&) -> std::ostream &;

  struct standard_error_port : public std::ostream
  {
    explicit standard_error_port();
  };

  auto operator <<(std::ostream &, standard_error_port const&) -> std::ostream &;

  #define DEFINE(TYPENAME, BASE)                                               \
  struct TYPENAME : public BASE                                                \
  {                                                                            \
    std::string const name;                                                    \
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
