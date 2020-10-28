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

  #define BOILERPLATE(TYPENAME, BASE)                                          \
  struct TYPENAME                                                              \
    : public std::BASE                                                         \
  {                                                                            \
    const path pathname;                                                       \
                                                                               \
    using std::BASE::BASE;                                                     \
                                                                               \
    explicit TYPENAME(const std::string& pathname)                             \
      : std::BASE { pathname }                                                 \
      , pathname { pathname }                                                  \
    {}                                                                         \
  };                                                                           \
                                                                               \
  auto operator <<(std::ostream& port, const TYPENAME&) -> decltype(port)

  BOILERPLATE( input_port, ifstream);
  BOILERPLATE(output_port, ofstream);

  #undef BOILERPLATE
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_PORT_HPP
