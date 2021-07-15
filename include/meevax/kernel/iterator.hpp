#ifndef INCLUDED_MEEVAX_KERNEL_ITERATOR_HPP
#define INCLUDED_MEEVAX_KERNEL_ITERATOR_HPP

#include <iterator> // std::begin, std::end, std::distance

#include <meevax/kernel/pair.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename T>
  struct homoiconic_iterator
  #ifdef MEEVAX_HOMOICONIC_ITERATOR_USE_REFERENCE_WRAPPER
    : public std::reference_wrapper<T>
  #else
    : public let
  #endif
  {
    using iterator_category = std::forward_iterator_tag;

    #ifdef MEEVAX_HOMOICONIC_ITERATOR_USE_REFERENCE_WRAPPER
    using value_type = std::reference_wrapper<T>;
    #else
    using value_type = let;
    #endif

    using reference = typename std::add_lvalue_reference<value_type>::type;

    using const_reference = typename std::add_const<reference>::type;

    using pointer = value_type; // homoiconicity

    using difference_type = std::ptrdiff_t;

    using size_type = std::size_t;

    #ifdef MEEVAX_HOMOICONIC_ITERATOR_USE_REFERENCE_WRAPPER
    using std::reference_wrapper<T>::reference_wrapper;

    homoiconic_iterator(T const& x)
      : std::reference_wrapper<T> { std::cref(x) }
    {}
    #else
    template <typename... Ts>
    constexpr homoiconic_iterator(Ts&&... xs)
      : let { std::forward<decltype(xs)>(xs)... }
    {}
    #endif

    decltype(auto) operator *() const
    {
      return car(*this);
    }

    decltype(auto) operator ->() const
    {
      return operator *();
    }

    decltype(auto) operator ++()
    {
      #ifdef MEEVAX_HOMOICONIC_ITERATOR_USE_REFERENCE_WRAPPER
      static_cast<let &>(*this) = cdr(*this);
      return *this;
      #else
      return *this = cdr(*this);
      #endif
    }

    auto operator ++(int)
    {
      auto copy = *this;
      operator++();
      return copy;
    }

    #ifdef MEEVAX_HOMOICONIC_ITERATOR_USE_REFERENCE_WRAPPER
    operator T&() const noexcept
    {
      return std::reference_wrapper<T>::get();
    }

    using std::reference_wrapper<T>::get;

    decltype(auto) operator==(homoiconic_iterator const& rhs) const noexcept { return get() == rhs.get(); }
    decltype(auto) operator!=(homoiconic_iterator const& rhs) const noexcept { return get() != rhs.get(); }

    operator bool() const
    {
      return static_cast<bool>(static_cast<T const&>(*this));
    }
    #endif

    homoiconic_iterator cbegin() const noexcept { return *this; }
    homoiconic_iterator  begin() const noexcept { return *this; }
    homoiconic_iterator   cend() const noexcept { return unit; }
    homoiconic_iterator    end() const noexcept { return unit; }
  };
} // namespace kernel
} // namespace meevax

namespace std
{
  auto cbegin(meevax::let const& x) -> meevax::homoiconic_iterator<meevax::let const>;
  auto  begin(meevax::let const& x) -> meevax::homoiconic_iterator<meevax::let const>;
  auto   cend(meevax::let const&  ) -> meevax::homoiconic_iterator<meevax::let const>;
  auto    end(meevax::let const&  ) -> meevax::homoiconic_iterator<meevax::let const>;
} // namespace std

#endif // INCLUDED_MEEVAX_KERNEL_ITERATOR_HPP
