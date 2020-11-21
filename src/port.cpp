#include <meevax/kernel/port.hpp>
#include <meevax/posix/vt102.hpp>

namespace meevax { inline namespace kernel
{
  #define BOILERPLATE(TYPENAME, PORTTYPE)                                      \
  auto operator<<(std::ostream& port, const TYPENAME& datum) -> decltype(port) \
  {                                                                            \
    port << magenta << "#,(" << green << "open-" PORTTYPE << " " << datum.name << reset; \
                                                                               \
    if (not datum.is_open())                                                   \
    {                                                                          \
      port << faint << " #;closed" << reset;                                   \
    }                                                                          \
                                                                               \
    return port << magenta << ")" << reset;                                    \
  }

  BOILERPLATE( input_file_port,  "input-file");
  BOILERPLATE(output_file_port, "output-file");

  #undef BOILERPLATE

  #define BOILERPLATE(TYPENAME, PORTTYPE)                                      \
  auto operator<<(std::ostream& port, const TYPENAME&) -> decltype(port)       \
  {                                                                            \
    return port << magenta << "#,(" << green << "open-" PORTTYPE << magenta << ")" << reset; \
  }

  BOILERPLATE( input_string_port,  "input-string");
  BOILERPLATE(output_string_port, "output-string");

  #undef BOILERPLATE
}} // namespace meevax::kernel
