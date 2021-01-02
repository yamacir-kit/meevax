#include <meevax/kernel/number.hpp>
#include <meevax/posix/vt102.hpp>

namespace meevax
{
inline namespace kernel
{
  auto ratio::numerator() const -> exact_integer const&
  {
    return std::get<0>(*this).as<exact_integer>();
  }

  auto ratio::denominator() const -> exact_integer const&
  {
    return std::get<1>(*this).as<exact_integer>();
  }

  auto ratio::is_integer() const -> bool
  {
    return cdr(*this).as<exact_integer>() == 1;
  }

  auto ratio::reduce() const -> ratio
  {
    using boost::multiprecision::gcd;

    if (const exact_integer divisor {
          gcd(car(*this).as<exact_integer>().value,
              cdr(*this).as<exact_integer>().value) }; divisor != 1)
    {
      return ratio(make<exact_integer>(car(*this).as<exact_integer>().value / divisor.value),
                   make<exact_integer>(cdr(*this).as<exact_integer>().value / divisor.value));
    }
    else
    {
      return *this;
    }
  }

  auto operator <<(output_port & port, ratio const& datum) -> output_port &
  {
    return port << cyan << car(datum)
                << cyan << "/"
                << cyan << cdr(datum) << reset;
  }
} // namespace kernel
} // namespace meevax
