#ifndef INCLUDED_MEEVAX_KERNEL_FLOATING_POINT_HPP
#define INCLUDED_MEEVAX_KERNEL_FLOATING_POINT_HPP

#include <meevax/kernel/pair.hpp>

namespace meevax { inline namespace kernel
{
  /* ---- Floating Point Number ------------------------------------------------
   *
   * ------------------------------------------------------------------------ */
  template <typename T>
  struct floating_point;

  #define BOILERPLATE(TYPE)                                                    \
  template <>                                                                  \
  struct floating_point<TYPE>                                                  \
    : public std::numeric_limits<TYPE>                                         \
  {                                                                            \
    using value_type = TYPE;                                                   \
                                                                               \
    value_type value;                                                          \
                                                                               \
    template <typename... Ts>                                                  \
    explicit constexpr floating_point(Ts&&... xs)                              \
      : value { boost::lexical_cast<value_type>(std::forward<decltype(xs)>(xs)...) } \
    {}                                                                         \
                                                                               \
    template <typename T,                                                      \
              typename =                                                       \
                typename std::enable_if<                                       \
                  std::is_convertible<T, value_type>::value                    \
                >::type>                                                       \
    explicit constexpr floating_point(T&& x)                                   \
      : value { x }                                                            \
    {}                                                                         \
                                                                               \
    auto to_string() const -> std::string                                      \
    {                                                                          \
      return boost::lexical_cast<std::string>(value);                          \
    }                                                                          \
                                                                               \
    auto exact() const noexcept                                                \
    {                                                                          \
      return value == std::trunc(value);                                       \
    }                                                                          \
                                                                               \
    constexpr operator value_type() const noexcept { return value; }           \
    constexpr operator value_type()       noexcept { return value; }           \
                                                                               \
    auto operator * (const object&) const -> object;                           \
    auto operator + (const object&) const -> object;                           \
    auto operator - (const object&) const -> object;                           \
    auto operator / (const object&) const -> object;                           \
                                                                               \
    auto operator ==(const object&) const -> object;                           \
    auto operator !=(const object&) const -> object;                           \
    auto operator < (const object&) const -> object;                           \
    auto operator <=(const object&) const -> object;                           \
    auto operator > (const object&) const -> object;                           \
    auto operator >=(const object&) const -> object;                           \
  }

  BOILERPLATE(float);
  BOILERPLATE(double);

  #undef BOILERPLATE

  using single_float = floating_point<float>;
  using double_float = floating_point<double>;

  using most_precise = double;

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
      return os << cyan << rhs.value << (rhs.exact() ? ".0" : "") << reset;
    }
  }

  #define BOILERPLATE(SYMBOL)                                                  \
  template <typename T, typename U>                                            \
  constexpr auto operator SYMBOL(const floating_point<T>& lhs, const floating_point<U>& rhs) \
  {                                                                            \
    using result_type = decltype(std::declval<T>() SYMBOL std::declval<U>());  \
    return floating_point<result_type>(lhs.value SYMBOL rhs.value);            \
  } static_assert(true)

  BOILERPLATE(*);
  BOILERPLATE(+);
  BOILERPLATE(-);
  BOILERPLATE(/);

  #undef BOILERPLATE

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
