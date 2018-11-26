#ifndef INCLUDED_MEEVAX_LISP_ITERATOR_HPP
#define INCLUDED_MEEVAX_LISP_ITERATOR_HPP

#include <memory>
#include <utility>

namespace meevax::lisp
{
  std::size_t n {0};

  template <typename T>
  struct iterator
    : public std::shared_ptr<T>,
      public std::iterator<std::input_iterator_tag, typename std::shared_ptr<T>::element_type>
  {
    template <typename... Ts>
    constexpr iterator(Ts&&... args)
      : std::shared_ptr<T> {std::forward<Ts>(args)...}
    {
      ++n;
    }

    template <typename... Ts>
    constexpr decltype(auto) operator=(Ts&&... args)
    {
      ++n;
      return std::shared_ptr<T>::operator=(std::forward<Ts>(args)...);
    }

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
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_ITERATOR_HPP

