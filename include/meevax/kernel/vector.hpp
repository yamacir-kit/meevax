#ifndef INCLUDED_MEEVAX_KERNEL_VECTOR_HPP
#define INCLUDED_MEEVAX_KERNEL_VECTOR_HPP

#include <meevax/kernel/list.hpp>

namespace meevax { inline namespace kernel
{
  enum class in_range_tag {} static constexpr in_range {};

  struct vector
    : public std::vector<object>
  {
    template <typename... Ts>
    explicit vector(Ts&&... xs)
      : std::vector<object> { std::forward<decltype(xs)>(xs)... }
    {}

    explicit vector(in_range_tag, homoiconic_iterator begin, homoiconic_iterator end)
      : std::vector<object> {}
    {
      std::copy(begin, end, std::back_inserter(*this));
    }

    explicit vector(in_range_tag, const value_type& xs)
      : std::vector<object> {}
    {
      std::copy(std::begin(xs), std::end(xs), std::back_inserter(*this));
    }
  };

  auto operator ==(const vector&, const vector&) -> bool;

  auto operator <<(std::ostream& port, const vector&) -> decltype(port);
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_VECTOR_HPP
