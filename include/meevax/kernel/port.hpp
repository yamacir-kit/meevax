#ifndef INCLUDED_MEEVAX_KERNEL_PORT_HPP
#define INCLUDED_MEEVAX_KERNEL_PORT_HPP

#include <fstream>

#include <meevax/kernel/object.hpp>

namespace meevax::kernel
{
  /* ==== Ports ================================================================
  *
  * TODO std::string => std::filesystem::path
  * TODO null-port
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
      os << posix::highlight::syntax << "#,("                   \
         << posix::highlight::type   << NAME;                                  \
                                                                               \
      if (port.is_open())                                                      \
      {                                                                        \
        os << posix::highlight::datum << " " << std::quoted(port.name);        \
      }                                                                        \
                                                                               \
      return os << posix::highlight::syntax << ")"                             \
                << posix::attribute::normal;                                   \
    }                                                                          \
  }

  DEFINE_PORT(       port,        "port",  fstream);
  DEFINE_PORT( input_port,  "input-port", ifstream);
  DEFINE_PORT(output_port, "output-port", ofstream);
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_PORT_HPP

