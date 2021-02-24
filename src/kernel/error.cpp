#include <meevax/kernel/error.hpp>

namespace meevax
{
inline namespace kernel
{
  auto operator <<(output_port & port, error_ const& datum) -> output_port &
  {
    port << magenta << "#,(" << green << "error " << reset << car(datum);

    for (let const& each : cdr(datum))
    {
      port << " " << each;
    }

    return port << magenta << ")" << reset;
  }
} // namespace kernel
} // namespace meevax
