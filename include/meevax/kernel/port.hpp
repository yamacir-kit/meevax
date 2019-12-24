#ifndef INCLUDED_MEEVAX_KERNEL_PORT_HPP
#define INCLUDED_MEEVAX_KERNEL_PORT_HPP

#include <fstream>

#include <meevax/kernel/object.hpp>

namespace meevax::kernel
{
  /* ==== Ports ================================================================
  *
  * TODO std::string => std::filesystem::path
  *
  *========================================================================== */
  #define DEFINE_PORT(IDENTIFIER, NAME, BASE)                                  \
  struct IDENTIFIER                                                            \
    : public std::BASE                                                         \
  {                                                                            \
    const std::string name;                                                    \
                                                                               \
    using std::BASE::BASE;                                                     \
                                                                               \
    explicit IDENTIFIER(const std::string& name)                               \
      : std::BASE {name}                                                       \
      , name {name}                                                            \
    {}                                                                         \
                                                                               \
    friend auto operator<<(std::ostream& os, const IDENTIFIER& port)           \
      -> decltype(os)                                                          \
    {                                                                          \
      os << highlight::syntax << external_form << "("                          \
         << highlight::constructor << NAME;                                    \
                                                                               \
      if (port.is_open())                                                      \
      {                                                                        \
        os << highlight::literal << " \"" << port.name << "\"";                \
      }                                                                        \
                                                                               \
      return os << highlight::syntax << ")" << attribute::normal;              \
    }                                                                          \
  }

  DEFINE_PORT(       port,        "port", fstream);
  DEFINE_PORT( input_port,  "input-port", ifstream);
  DEFINE_PORT(output_port, "output-port", ofstream);
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_PORT_HPP

