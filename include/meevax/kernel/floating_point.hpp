#ifndef INCLUDED_MEEVAX_KERNEL_FLOATING_POINT_HPP
#define INCLUDED_MEEVAX_KERNEL_FLOATING_POINT_HPP

#include <meevax/kernel/pair.hpp>

namespace meevax { inline namespace kernel
{
  using most_precise = double;

  /* ---- Floating Point Number ------------------------------------------------
   *
   * ------------------------------------------------------------------------ */
  template <typename T>
  struct floating_point
    : public std::numeric_limits<T>
  {
    using value_type = T;

    value_type value;

    explicit constexpr floating_point(T value = {})
      : value { value }
    {}

    template <typename U, typename = typename std::enable_if<std::is_convertible<U, value_type>::value>::type>
    explicit constexpr floating_point(U&& x)
      : value { x }
    {}

    template <typename... Ts>
    explicit constexpr floating_point(Ts&&... xs)
      : value { boost::lexical_cast<value_type>(std::forward<decltype(xs)>(xs)...) }
    {}

    auto to_string() const
    {
      return boost::lexical_cast<std::string>(value);
    }

    constexpr auto is_exact() const noexcept
    {
      return value == std::trunc(value);
    }

    constexpr auto is_inexact() const noexcept
    {
      return not is_exact();
    }

    auto as_exact() const;

    constexpr auto as_inexact() const noexcept;

    constexpr operator value_type() const noexcept { return value; }
    constexpr operator value_type()       noexcept { return value; }

    auto operator * (const object&) const -> object;
    auto operator + (const object&) const -> object;
    auto operator - (const object&) const -> object;
    auto operator / (const object&) const -> object;
    auto operator % (const object&) const -> object;

    auto operator ==(const object&) const -> bool;
    auto operator !=(const object&) const -> bool;
    auto operator < (const object&) const -> bool;
    auto operator <=(const object&) const -> bool;
    auto operator > (const object&) const -> bool;
    auto operator >=(const object&) const -> bool;
  };

  template struct floating_point<float>;
  template struct floating_point<double>;
  template struct floating_point<long double>;

  using single_float = floating_point<float>;
  using double_float = floating_point<double>;

  using default_float = floating_point<decltype(0.0)>;

  template <typename T>
  auto operator <<(std::ostream& os, const floating_point<T>& rhs) -> decltype(auto)
  {
    if (std::isnan(rhs))
    {
      return os << cyan << "+nan.0" << reset;
    }
    else if (std::isinf(rhs))
    {
      return os << cyan << (0 < rhs.value ? '+' : '-') << "inf.0" << reset;
    }
    else
    {
      return os << cyan << (rhs.is_exact() ? "#i" : "") << rhs.value << reset;
    }
  }

  #define BOILERPLATE(SYMBOL, OPERATION)                                       \
  template <typename T, typename U>                                            \
  constexpr auto operator SYMBOL(const floating_point<T>& lhs, const floating_point<U>& rhs) \
  {                                                                            \
    return floating_point(OPERATION(lhs.value, rhs.value));                    \
  } static_assert(true)

  BOILERPLATE(*, std::multiplies<void>());
  BOILERPLATE(+, std::plus<void>());
  BOILERPLATE(-, std::minus<void>());
  BOILERPLATE(/, std::divides<void>());
  BOILERPLATE(%, std::fmod);

  #undef BOILERPLATE

  // template <typename T, typename U>
  // constexpr auto operator %(const floating_point<T>& lhs, const floating_point<U>& rhs)
  // {
  //   return floating_point(std::fmod(lhs.value, rhs.value));
  // }

  #define BOILERPLATE(SYMBOL)                                                  \
  template <typename T, typename U>                                            \
  constexpr auto operator SYMBOL(const floating_point<T>& lhs, const floating_point<U>& rhs) \
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
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_FLOATING_POINT_HPP
