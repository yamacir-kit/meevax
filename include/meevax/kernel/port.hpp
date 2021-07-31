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

#include <ios>
#include <fstream>

#include <meevax/kernel/path.hpp>

namespace meevax
{
inline namespace kernel
{
  void copy_ios(std::ios & from, std::ios & to);

  struct standard_input_port : public input_port
  {
    explicit standard_input_port()
    {
      copy_ios(std::cin, *this);
    }
  };

  struct standard_output_port : public output_port
  {
    explicit standard_output_port()
    {
      copy_ios(std::cout, *this);
    }
  };

  struct standard_error_port : public output_port
  {
    explicit standard_error_port()
    {
      copy_ios(std::cerr, *this);
    }
  };

  let extern const default_input_port;
  let extern const default_output_port;
  let extern const default_error_port;

  auto operator <<(output_port &, standard_input_port  const&) -> output_port &;
  auto operator <<(output_port &, standard_output_port const&) -> output_port &;
  auto operator <<(output_port &, standard_error_port  const&) -> output_port &;

  /* ---- File Ports -----------------------------------------------------------
   *
   *
   * ------------------------------------------------------------------------ */
  template <typename T>
  struct file_port : public T
  {
    path const name;

    explicit file_port(std::string const& name)
      : T    { name }
      , name { name }
    {}
  };

  using  input_file_port = file_port<std::ifstream>;
  using output_file_port = file_port<std::ofstream>;

  auto operator <<(output_port &,  input_file_port const&) -> output_port &;
  auto operator <<(output_port &, output_file_port const&) -> output_port &;

  /* ---- String Ports ---------------------------------------------------------
   *
   *  SRFI-6
   *
   * ------------------------------------------------------------------------ */
  struct input_string_port : public std::istringstream
  {
    using std::istringstream::istringstream;
  };

  struct output_string_port : public std::ostringstream
  {
    using std::ostringstream::ostringstream;
  };

  auto operator <<(output_port & port,  input_string_port const&) -> output_port &;
  auto operator <<(output_port & port, output_string_port const&) -> output_port &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PORT_HPP
