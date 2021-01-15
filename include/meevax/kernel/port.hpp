#ifndef INCLUDED_MEEVAX_KERNEL_PORT_HPP
#define INCLUDED_MEEVAX_KERNEL_PORT_HPP

#include <fstream>
#include <sstream>

#include <meevax/kernel/path.hpp>
#include <meevax/kernel/preface.hpp>

namespace meevax
{
inline namespace kernel
{
  /* ---- Ports ----------------------------------------------------------------
   *
   *
   * ------------------------------------------------------------------------ */

  using  input_port = std::istream;
  using output_port = std::ostream;

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
      copyfmt(ios);                                                            \
      clear(ios.rdstate());                                                    \
      static_cast<std::ios&>(*this).rdbuf(ios.rdbuf());                        \
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
