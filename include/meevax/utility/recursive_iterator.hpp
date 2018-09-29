#ifndef INCLUDED_MEEVAX_UTILITY_RECURSIVE_ITERATOR_HPP_HPP
#define INCLUDED_MEEVAX_UTILITY_RECURSIVE_ITERATOR_HPP_HPP

#include <iterator>
#include <memory>
#include <utility>

namespace meevax::utility
{
  template <typename T>
  struct recursive_iterator
    : public std::shared_ptr<T>
  {
    using iterator_category = std::forward_iterator_tag;
    using difference_type = std::ptrdiff_t;
    using value_type = recursive_iterator<T>;
    using pointer = value_type;
    using reference = value_type&;

    template <typename... Ts>
    constexpr recursive_iterator(Ts&&... args) noexcept
      : std::shared_ptr<T> {std::forward<Ts>(args)...}
    {}

    reference operator*() const noexcept
    {
      return std::shared_ptr<T>::get()->first;
    }

    reference operator++() noexcept
    {
      return *this = std::shared_ptr<T>::get()->second;
    }
  };
} // namespace meevax::utility

#endif // INCLUDED_MEEVAX_UTILITY_RECURSIVE_ITERATOR_HPP_HPP

