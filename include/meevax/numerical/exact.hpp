#ifndef INCLUDED_MEEVAX_NUMERICAL_EXACT_HPP
#define INCLUDED_MEEVAX_NUMERICAL_EXACT_HPP

namespace meevax::numerical::exact
{
  template <typename T>
  inline constexpr T log2(T&& k) noexcept
  {
    return (k < 2) ? 0 : 1 + log2(std::forward<decltype(k)>(k) / 2);
  };
} // namespace meevax::numerical::exact

#endif // INCLUDED_MEEVAX_NUMERICAL_EXACT_HPP

