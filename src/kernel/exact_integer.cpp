#include <meevax/kernel/exact_integer.hpp>
#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace kernel
{
  let const e0 = make<exact_integer>(0);
  let const e1 = make<exact_integer>(1);

  auto operator <<(output_port & port, exact_integer const& datum) -> output_port &
  {
    return port << cyan << datum.to_string() << reset;
  }
} // namespace kernel
} // namespace meevax
