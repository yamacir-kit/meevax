#ifndef INCLUDED_MEEVAX_KERNEL_FILE_HPP
#define INCLUDED_MEEVAX_KERNEL_FILE_HPP

#include <fstream>

#include <meevax/kernel/object.hpp>

namespace meevax::kernel
{
  /* ==== I/O Port ============================================================
  *
  * TODO std::string => std::filesystem::path
  *
  *========================================================================= */
  #define DEFINE_PORT(TYPENAME, STREAM)                                        \
  struct TYPENAME                                                              \
    : public STREAM                                                            \
  {                                                                            \
    const std::string name;                                                    \
                                                                               \
    explicit TYPENAME(const std::string& name)                                 \
      : STREAM {name}                                                          \
      , name   {name}                                                          \
    {}                                                                         \
                                                                               \
    template <typename... Ts>                                                  \
    explicit TYPENAME(Ts&&... operands)                                        \
      : STREAM {std::forward<decltype(operands)>(operands)...}                 \
    {}                                                                         \
  }

  /* ==== I/O Port External Representation ====================================
  *
  *
  *========================================================================= */
  #define DEFINE_PORT_EXTERNAL_REPRESENTATION(TYPENAME, DISPLAY_NAME)          \
  auto operator<<(std::ostream& os, const TYPENAME& port)                      \
    -> decltype(os)                                                            \
  {                                                                            \
    os << "#(" << DISPLAY_NAME;                                                \
                                                                               \
    if (port.is_open())                                                        \
    {                                                                          \
      os << " \"" << port.name << "\"";                                        \
    }                                                                          \
                                                                               \
    return os << ")";                                                          \
  }

  DEFINE_PORT(       file, std::fstream);
  DEFINE_PORT( input_file, std::ifstream);
  DEFINE_PORT(output_file, std::ofstream);

  DEFINE_PORT_EXTERNAL_REPRESENTATION(       file,        "file")
  DEFINE_PORT_EXTERNAL_REPRESENTATION( input_file,  "input-file")
  DEFINE_PORT_EXTERNAL_REPRESENTATION(output_file, "output-file")
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_FILE_HPP

