#ifndef INCLUDED_MEEVAX_KERNEL_VECTOR_HPP
#define INCLUDED_MEEVAX_KERNEL_VECTOR_HPP

#include <meevax/kernel/list.hpp>

namespace meevax::kernel
{
  enum class in_range_tag {} in_range;

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

    friend auto operator<<(std::ostream& os, const vector& v) -> decltype(os)
    {
      os << console::magenta << "#(" << console::reset;

      for (auto iter { std::begin(v) }; iter != std::end(v); ++iter)
      {
        os << *iter << (std::next(iter) != std::end(v) ? " " : "");
      }

      return os << console::magenta << ")" << console::reset;
    }
  };

  static_assert(not std::is_base_of<pair, vector>::value);
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_VECTOR_HPP
