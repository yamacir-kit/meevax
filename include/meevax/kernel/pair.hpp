#ifndef INCLUDED_MEEVAX_KERNEL_PAIR_HPP
#define INCLUDED_MEEVAX_KERNEL_PAIR_HPP

#include <meevax/kernel/object.hpp>

namespace meevax
{
inline namespace kernel
{
  let extern const unit;

  /* ---- Pair -----------------------------------------------------------------
   *
   *
   * ------------------------------------------------------------------------ */
  struct pair
    : public std::pair<let, let>
    , public top<pair>
  {
    explicit pair(let const& a = unit,
                  let const& b = unit)
      : std::pair<let, let> { a, b }
    {}

    virtual ~pair() = default;
  };

  auto operator <<(output_port & port, pair const&) -> output_port &;

  /* ---- Pair Accessor --------------------------------------------------------
   *
   *  Pair accessors are not only for pair type. Accessing car and cdr is a
   *  valid operation for everyone except the empty list.
   *
   * ------------------------------------------------------------------------ */
  auto car = [](auto&& x) noexcept -> decltype(auto) { return std::get<0>(unwrap(std::forward<decltype(x)>(x))); };
  auto cdr = [](auto&& x) noexcept -> decltype(auto) { return std::get<1>(unwrap(std::forward<decltype(x)>(x))); };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PAIR_HPP
