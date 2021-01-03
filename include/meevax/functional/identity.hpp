#ifndef INCLUDED_MEEVAX_FUNCTIONAL_IDENTITY_HPP
#define INCLUDED_MEEVAX_FUNCTIONAL_IDENTITY_HPP

#include <type_traits>

namespace meevax
{
inline namespace functional
{
  template <typename T>
  struct identity
  {
    using type = T;

    using is_transparent = std::true_type;

    constexpr decltype(auto) operator ()(T&& x) const noexcept
    {
      return std::forward<decltype(x)>(x);
    }
  };
} // namespace functional
} // namespace meevax

#endif // INCLUDED_MEEVAX_FUNCTIONAL_IDENTITY_HPP
