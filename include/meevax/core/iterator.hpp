#ifndef INCLUDED_MEEVAX_CORE_ITERATOR_HPP
#define INCLUDED_MEEVAX_CORE_ITERATOR_HPP

#include <memory>
#include <utility>

namespace meevax::core
{
  template <typename T>
  struct iterator
    : public std::shared_ptr<T>,
      public std::iterator<std::input_iterator_tag, typename std::shared_ptr<T>::element_type>
  {
    template <typename... Ts>
    constexpr iterator(Ts&&... args)
      : std::shared_ptr<T> {std::forward<Ts>(args)...}
    {}

    friend decltype(auto) car(const std::shared_ptr<T>& iter) noexcept
    {
      return std::get<0>(iter.std::shared_ptr<T>::operator*());
    }

    friend decltype(auto) cdr(const std::shared_ptr<T>& iter) noexcept
    {
      return std::get<1>(iter.std::shared_ptr<T>::operator*());
    }

    decltype(auto) operator*() const noexcept
    {
      return car(*this);
    }

    decltype(auto) operator++() noexcept
    {
      return *this = cdr(*this);
    }
  };
} // namespace meevax::core

#endif // INCLUDED_MEEVAX_CORE_ITERATOR_HPP

