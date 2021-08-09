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
#include <ios>

#include <meevax/kernel/object.hpp>
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

  template <typename T>
  struct file_port : public T
  {
    path const name;

    explicit file_port(std::string const& name)
      : T    { name }
      , name { name }
    {}
  };

  using input_file_port = file_port<std::ifstream>;

  auto operator <<(std::ostream &, input_file_port const&) -> std::ostream &;

  using output_file_port = file_port<std::ofstream>;

  auto operator <<(std::ostream &, output_file_port const&) -> std::ostream &;

  /* ---- String Ports ---------------------------------------------------------
   *
   *  SRFI-6
   *
   * ------------------------------------------------------------------------ */
  struct input_string_port : public std::istringstream
  {
    using std::istringstream::istringstream;
  };

  auto operator <<(std::ostream &, input_string_port const&) -> std::ostream &;

  struct output_string_port : public std::ostringstream
  {
    using std::ostringstream::ostringstream;
  };

  auto operator <<(std::ostream &, output_string_port const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PORT_HPP
