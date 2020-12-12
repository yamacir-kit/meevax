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

  #define BOILERPLATE(SYMBOL)                                                  \
  auto operator SYMBOL(const exact_integer& lhs, const exact_integer& rhs) -> exact_integer \
  {                                                                            \
    return exact_integer(lhs.value SYMBOL rhs.value);                          \
  } static_assert(true)

  BOILERPLATE(*);
  BOILERPLATE(+);
  BOILERPLATE(-);
  BOILERPLATE(/);
  BOILERPLATE(%);

  #undef BOILERPLATE

  #define BOILERPLATE(SYMBOL)                                                  \
  auto operator SYMBOL(const exact_integer& lhs, const exact_integer& rhs) -> bool \
  {                                                                            \
    return lhs.value SYMBOL rhs.value;                                         \
  } static_assert(true)

  BOILERPLATE(!=);
  BOILERPLATE(<);
  BOILERPLATE(<=);
  BOILERPLATE(==);
  BOILERPLATE(>);
  BOILERPLATE(>=);

  #undef BOILERPLATE
} // namespace kernel
} // namespace meevax
