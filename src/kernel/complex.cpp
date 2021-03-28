#include <meevax/kernel/complex.hpp>
#include <meevax/kernel/number.hpp>
#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace kernel
{
  auto operator <<(output_port & port, complex const& z) -> output_port &
  {
    let static const zero = make<exact_integer>(0);

    return port << cyan << z.real() << (zero < z.imag() ? '+' : '-') << z.imag() << "i" << reset;
  }
} // namespace kernel
} // namespace meevax
