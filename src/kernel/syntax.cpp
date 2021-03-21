#include <meevax/kernel/port.hpp>
#include <meevax/kernel/syntax.hpp>
#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace kernel
{
  auto operator <<(output_port & port, syntax const& datum) -> output_port &
  {
    return port << magenta << "#,(" << green << "syntax" << reset << " " << datum.name << magenta << ")" << reset;
  }
} // namespace kernel
} // namespace meevax
