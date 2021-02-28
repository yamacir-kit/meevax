#include <meevax/kernel/procedure.hpp>
#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace kernel
{
  auto operator <<(output_port & port, procedure const& datum) -> output_port &
  {
    return port << magenta << "#,(" << green << "procedure " << reset << datum.name << magenta << ")" << reset;
  }
} // namespace kernel
} // namespace meevax
