#include <meevax/kernel/continuation.hpp>
#include <meevax/kernel/port.hpp>
#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace kernel
{
  auto operator <<(output_port & port, continuation const& datum) -> output_port &
  {
    return port << magenta << "#,("
                << green << "continuation" << reset
                << faint << " ;#" << &datum << reset
                << magenta << ")" << reset;
  }
} // namespace kernel
} // namespace meevax
