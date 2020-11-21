#ifndef INCLUDED_MEEVAX_KERNEL_PORT_HPP
#define INCLUDED_MEEVAX_KERNEL_PORT_HPP

#include <fstream>
#include <sstream>

#include <meevax/kernel/path.hpp>

namespace meevax { inline namespace kernel
{
  /* ---- Ports ----------------------------------------------------------------
   *
   *  TODO null-port
   *
   * ------------------------------------------------------------------------ */

  #define BOILERPLATE(TYPENAME, STREAM)                                        \
  struct TYPENAME                                                              \
    : public std::STREAM                                                       \
  {                                                                            \
    const path name;                                                           \
                                                                               \
    explicit TYPENAME(const std::string& name)                                 \
      : std::STREAM { name }                                                   \
      , name { name }                                                          \
    {}                                                                         \
                                                                               \
    explicit TYPENAME(const std::string& name, const std::ios& ios)            \
      : std::STREAM { name }                                                   \
      , name { name }                                                          \
    {                                                                          \
      copyfmt(ios);                                                            \
      clear(ios.rdstate());                                                    \
    }                                                                          \
  };                                                                           \
                                                                               \
  auto operator <<(std::ostream& port, const TYPENAME&) -> decltype(port)

  using  input_port = std::istream;
  using output_port = std::ostream;

  BOILERPLATE( input_file_port, ifstream);
  BOILERPLATE(output_file_port, ofstream);

  #undef BOILERPLATE

  struct input_string_port
    : public std::istringstream
  {
    using std::istringstream::istringstream;
  };

  struct output_string_port
    : public std::ostringstream
  {
    using std::ostringstream::ostringstream;
  };
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_PORT_HPP
