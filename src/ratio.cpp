#include <meevax/kernel/number.hpp>
#include <meevax/kernel/ratio.hpp>
#include <meevax/posix/vt102.hpp>

namespace meevax
{
inline namespace kernel
{
  auto ratio::is_integer() const -> bool
  {
    return cdr(*this).as<exact_integer>() == 1;
  }

  auto ratio::reduce() const -> ratio
  {
    if (const exact_integer divisor {
          boost::multiprecision::gcd(
            car(*this).as<exact_integer>().value,
            cdr(*this).as<exact_integer>().value)
        }; divisor != 1)
    {
      return ratio(
        make(car(*this).as<exact_integer>() / divisor),
        make(cdr(*this).as<exact_integer>() / divisor));
    }
    else
    {
      return *this;
    }
  }

  auto ratio::reduce() -> ratio const&
  {
    if (const exact_integer divisor {
          boost::multiprecision::gcd(numerator().as<exact_integer>().value, denominator().as<exact_integer>().value)
        };
        divisor != 1)
    {
      car(*this) = make(car(*this).as<exact_integer>() / divisor);
      cdr(*this) = make(cdr(*this).as<exact_integer>() / divisor);
    }

    return *this;
  }

  auto operator <<(output_port & port, ratio const& datum) -> output_port &
  {
    return port << cyan << car(datum)
                << cyan << "/"
                << cyan << cdr(datum) << reset;
  }
} // namespace kernel
} // namespace meevax
