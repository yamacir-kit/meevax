#include <meevax/kernel/exact_integer.hpp>
#include <meevax/kernel/number.hpp>
#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace kernel
{
  auto operator <<(output_port & port, exact_integer const& datum) -> output_port &
  {
    return port << cyan << datum.to_string() << reset;
  }
} // namespace kernel
} // namespace meevax
