#ifndef INCLUDED_MEEVAX_KERNEL_VECTOR_HPP
#define INCLUDED_MEEVAX_KERNEL_VECTOR_HPP

#include <meevax/kernel/list.hpp>

namespace meevax
{
inline namespace kernel
{
  enum class in_range_tag {} static constexpr in_range {};

  struct vector
    : public std::vector<object>
  {
    template <typename... Ts>
    explicit vector(Ts&&... xs)
      : std::vector<object> { std::forward<decltype(xs)>(xs)... }
    {}

    template <typename InputIterator>
    explicit vector(in_range_tag, InputIterator&& begin,
                                  InputIterator&& end)
      : std::vector<object> {}
    {
      std::copy(begin, end, std::back_inserter(*this));
    }

    explicit vector(in_range_tag, value_type const& xs)
      : std::vector<object> {}
    {
      std::copy(std::cbegin(xs), std::cend(xs), std::back_inserter(*this));
    }
  };

  auto operator ==(vector const&, vector const&) -> bool;

  auto operator <<(std::ostream & port, vector const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_VECTOR_HPP
