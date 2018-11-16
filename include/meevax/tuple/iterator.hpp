#ifndef INCLUDED_MEEVAX_TUPLE_ITERATOR_HPP
#define INCLUDED_MEEVAX_TUPLE_ITERATOR_HPP

#include <iterator>
#include <memory>

namespace meevax::tuple
{
  template <typename T, auto N = 1>
  struct iterator
    : public std::shared_ptr<T>,
      public std::iterator<std::input_iterator_tag, typename std::shared_ptr<T>::element_type>
  {
    template <typename... Ts>
    constexpr iterator(Ts&&... args) noexcept
      : std::shared_ptr<T> {std::forward<Ts>(args)...}
    {}

    decltype(auto) operator*() const noexcept
    {
      return std::get<0>(std::shared_ptr<T>::operator*());
    }

    decltype(auto) operator++() noexcept
    {
      return *this = std::get<N>(std::shared_ptr<T>::operator*());
    }
  };
} // namespace meevax::tuple

#endif // INCLUDED_MEEVAX_TUPLE_ITERATOR_HPP

