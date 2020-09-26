#ifndef INCLUDED_MEEVAX_KERNEL_EXACT_INTEGER_HPP
#define INCLUDED_MEEVAX_KERNEL_EXACT_INTEGER_HPP

#ifndef MEEVAX_USE_GMP
#define MEEVAX_USE_GMP
#endif

#ifdef MEEVAX_USE_GMP
#include <boost/multiprecision/gmp.hpp>
#else
#include <boost/multiprecision/cpp_int.hpp>
#endif

#include <meevax/kernel/pair.hpp>

namespace meevax { inline namespace kernel
{
  /* ---- Multi-Presition Exact Integer ----------------------------------------
   *
   * ------------------------------------------------------------------------ */
  struct exact_integer
  {
    #ifdef MEEVAX_USE_GMP
    using value_type = boost::multiprecision::mpz_int;
    #else
    using value_type = boost::multiprecision::cpp_int;
    #endif

    value_type value;

    template <typename... Ts>
    explicit constexpr exact_integer(Ts&&... xs)
      : value { std::forward<decltype(xs)>(xs)... }
    {}

    auto to_string() const -> std::string
    {
      return value.str();
    }

    static constexpr auto exact() noexcept
    {
      return true;
    }

    operator value_type() const noexcept { return value; }
    operator value_type()       noexcept { return value; }

    auto operator * (const object&) const -> object;
    auto operator + (const object&) const -> object;
    auto operator - (const object&) const -> object;
    auto operator / (const object&) const -> object;

    auto operator ==(const object&) const -> object;
    auto operator !=(const object&) const -> object;
    auto operator < (const object&) const -> object;
    auto operator <=(const object&) const -> object;
    auto operator > (const object&) const -> object;
    auto operator >=(const object&) const -> object;
  };

  auto operator <<(std::ostream& os, const exact_integer& rhs) -> decltype(auto)
  {
    return os << cyan << rhs.value.str() << reset;
  }

  #define BOILERPLATE(SYMBOL)                                                  \
  auto operator SYMBOL(const exact_integer& lhs, const exact_integer& rhs)     \
  {                                                                            \
    return exact_integer(lhs.value SYMBOL rhs.value);                          \
  } static_assert(true)

  BOILERPLATE(*);
  BOILERPLATE(+);
  BOILERPLATE(-);
  BOILERPLATE(/);

  #undef BOILERPLATE

  #define BOILERPLATE(SYMBOL)                                                  \
  auto operator SYMBOL(const exact_integer& lhs, const exact_integer& rhs)     \
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

#endif // INCLUDED_MEEVAX_KERNEL_EXACT_INTEGER_HPP
