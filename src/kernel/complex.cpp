#include <meevax/kernel/complex.hpp>
#include <meevax/kernel/number.hpp>
#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace kernel
{
  auto operator <<(output_port & port, complex const& z) -> output_port &
  {
    return port << cyan << z.real() << (e0 < z.imag() ? '+' : '-') << z.imag() << "i" << reset;
  }
} // namespace kernel
} // namespace meevax
