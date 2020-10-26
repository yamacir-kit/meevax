#include <meevax/kernel/ratio.hpp>
#include <meevax/posix/vt102.hpp>

namespace meevax { inline namespace kernel
{
  auto operator <<(std::ostream& port, const ratio& rhs) -> decltype(port)
  {
    return port << cyan << car(rhs)
                << cyan << "/"
                << cyan << cdr(rhs) << reset;
  }

  auto operator +(const ratio& lhs, const ratio& rhs) -> ratio
  {
    return ratio(
      lhs.numerator() * rhs.denominator() + rhs.numerator() * lhs.denominator(),
      lhs.denominator() * rhs.denominator());
  }

  auto operator -(const ratio& lhs, const ratio& rhs) -> ratio
  {
    return ratio(
      lhs.numerator() * rhs.denominator() - rhs.numerator() * lhs.denominator(),
      lhs.denominator() * rhs.denominator());
  }

  auto operator *(const ratio& lhs, const ratio& rhs) -> ratio
  {
    return ratio(
      lhs.numerator() * rhs.numerator(),
      lhs.denominator() * rhs.denominator());
  }

  auto operator /(const ratio& lhs, const ratio& rhs) -> ratio
  {
    return lhs * rhs.invert();
  }

  #define BOILERPLATE(SYMBOL)                                                  \
  auto operator SYMBOL(const ratio& lhs, const ratio& rhs) -> bool             \
  {                                                                            \
    return (lhs.numerator() * rhs.denominator()).binding() SYMBOL (rhs.numerator() * lhs.denominator()); \
  } static_assert(true)

  BOILERPLATE(==);
  BOILERPLATE(!=);
  BOILERPLATE(<);
  BOILERPLATE(<=);
  BOILERPLATE(>);
  BOILERPLATE(>=);

  #undef BOILERPLATE
}} // namespace meevax::kernel
