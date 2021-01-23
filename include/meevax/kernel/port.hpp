#ifndef INCLUDED_MEEVAX_KERNEL_PORT_HPP
#define INCLUDED_MEEVAX_KERNEL_PORT_HPP

#include <ios>
#include <unistd.h>

#include <cassert>
#include <fstream>
#include <iostream>
#include <sstream>

#include <meevax/kernel/path.hpp>
#include <meevax/kernel/preface.hpp>

namespace meevax
{
inline namespace kernel
{
  using  input_port = std::istream;
  using output_port = std::ostream;

  /* ---- Standard Input Ports -------------------------------------------------
   *
   *
   * ------------------------------------------------------------------------ */
  struct standard_input
  {
    static constexpr auto fd = STDIN_FILENO;

    constexpr operator input_port &()
    {
      return std::cin;
    }
  };

  struct standard_output
  {
    static constexpr auto fd = STDOUT_FILENO;

    constexpr operator output_port &()
    {
      return std::cout;
    }
  };

  struct standard_error
  {
    static constexpr auto fd = STDERR_FILENO;

    constexpr operator output_port &()
    {
      return std::cerr;
    }
  };

  auto operator <<(output_port &, standard_input const&) -> output_port &;

  auto operator <<(output_port &, standard_output const&) -> output_port &;

  auto operator <<(output_port &, standard_error const&) -> output_port &;

  /* ---- Ports ----------------------------------------------------------------
   *
   *
   * ------------------------------------------------------------------------ */
  template <typename T>
  struct file_port
    : public T
  {
    const path name;

    explicit file_port(bytestring const& name)
      : T    { name }
      , name { name }
    {}

    explicit file_port(bytestring const& name, std::ios const& ios)
      : T    { name }
      , name { name }
    {
      std::ios::copyfmt(ios);
      std::ios::clear(ios.rdstate());
      std::ios::rdbuf(ios.rdbuf()); // NOTE: A very unstable and dirty hack.
    }
  };

  using input_file_port = file_port<std::ifstream>;
  using output_file_port = file_port<std::ofstream>;

  auto operator <<(output_port &, const input_file_port&) -> output_port &;
  auto operator <<(output_port &, const output_file_port&) -> output_port &;

  struct input_string_port
    : public std::istringstream
  {
    using std::istringstream::istringstream;
  };

  auto operator <<(std::ostream& port, const input_string_port&) -> decltype(port);

  struct output_string_port
    : public std::ostringstream
  {
    using std::ostringstream::ostringstream;
  };

  auto operator <<(std::ostream& port, const output_string_port&) -> decltype(port);
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PORT_HPP
