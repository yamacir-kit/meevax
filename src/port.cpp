#include <meevax/kernel/port.hpp>
#include <meevax/posix/vt102.hpp>

namespace meevax { inline namespace kernel
{
  #define BOILERPLATE(TYPENAME, FILETYPE)                                      \
  auto operator<<(std::ostream& port, const TYPENAME& datum) -> decltype(port) \
  {                                                                            \
    port << magenta << "#,(" << green << "open-" FILETYPE << " " << datum.pathname << reset; \
                                                                               \
    if (not datum.is_open())                                                   \
    {                                                                          \
      port << faint << " #;closed" << reset;                                   \
    }                                                                          \
                                                                               \
    return port << magenta << ")" << reset;                                    \
  }

  BOILERPLATE( input_port,  "input-file");
  BOILERPLATE(output_port, "output-file");

  #undef BOILERPLATE
}} // namespace meevax::kernel
