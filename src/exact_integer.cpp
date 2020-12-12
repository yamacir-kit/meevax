#include <meevax/kernel/exact_integer.hpp>
#include <meevax/kernel/port.hpp>
#include <meevax/posix/vt102.hpp>

namespace meevax
{
inline namespace kernel
{
  auto operator <<(output_port & port, exact_integer const& datum) -> output_port &
  {
    return port << cyan << datum.value.str() << reset;
  }

  auto operator * (exact_integer const& a, exact_integer const& b) -> exact_integer { return static_cast<exact_integer>(a.value * b.value); }
  auto operator + (exact_integer const& a, exact_integer const& b) -> exact_integer { return static_cast<exact_integer>(a.value + b.value); }
  auto operator - (exact_integer const& a, exact_integer const& b) -> exact_integer { return static_cast<exact_integer>(a.value - b.value); }
  auto operator / (exact_integer const& a, exact_integer const& b) -> exact_integer { return static_cast<exact_integer>(a.value / b.value); }
  auto operator % (exact_integer const& a, exact_integer const& b) -> exact_integer { return static_cast<exact_integer>(a.value % b.value); }

  auto operator !=(exact_integer const& a, exact_integer const& b) -> bool { return a.value != b.value; }
  auto operator < (exact_integer const& a, exact_integer const& b) -> bool { return a.value <  b.value; }
  auto operator <=(exact_integer const& a, exact_integer const& b) -> bool { return a.value <= b.value; }
  auto operator ==(exact_integer const& a, exact_integer const& b) -> bool { return a.value == b.value; }
  auto operator > (exact_integer const& a, exact_integer const& b) -> bool { return a.value >  b.value; }
  auto operator >=(exact_integer const& a, exact_integer const& b) -> bool { return a.value >= b.value; }
} // namespace kernel
} // namespace meevax
