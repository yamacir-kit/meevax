#ifndef INCLUDED_MEEVAX_KERNEL_PORT_HPP
#define INCLUDED_MEEVAX_KERNEL_PORT_HPP

#include <unistd.h>

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
  #define BOILERPLATE(TYPENAME, STREAM)                                        \
  struct TYPENAME                                                              \
    : public std::STREAM                                                       \
  {                                                                            \
    const path name;                                                           \
                                                                               \
    explicit TYPENAME(bytestring const& name)                                  \
      : std::STREAM { name }                                                   \
      , name { name }                                                          \
    {}                                                                         \
                                                                               \
    explicit TYPENAME(bytestring const& name, std::ios const& ios)             \
      : std::STREAM { name }                                                   \
      , name { name }                                                          \
    {                                                                          \
      std::ios::copyfmt(ios);                                                  \
      std::ios::clear(ios.rdstate());                                          \
      std::ios::rdbuf(ios.rdbuf());                                            \
    }                                                                          \
  };                                                                           \
                                                                               \
  auto operator <<(std::ostream& port, const TYPENAME&) -> decltype(port)

  BOILERPLATE( input_file_port, ifstream);
  BOILERPLATE(output_file_port, ofstream);

  #undef BOILERPLATE

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
