#ifndef INCLUDED_MEEVAX_KERNEL_PORT_HPP
#define INCLUDED_MEEVAX_KERNEL_PORT_HPP

#include <fstream>

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

  BOILERPLATE( input_port, ifstream);
  BOILERPLATE(output_port, ofstream);

  #undef BOILERPLATE
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_PORT_HPP
