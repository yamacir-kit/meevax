#ifndef INCLUDED_MEEVAX_UTILITY_RECURSIVE_TUPLE_ITERATOR_HPP
#define INCLUDED_MEEVAX_UTILITY_RECURSIVE_TUPLE_ITERATOR_HPP

#include <iterator>
#include <memory>
#include <utility>

#include <boost/iterator/iterator_facade.hpp>

namespace meevax::utility
{
  template <typename T>
  class recursive_tuple_iterator
    : public std::shared_ptr<T>,
      public boost::iterator_facade<recursive_tuple_iterator<T>, recursive_tuple_iterator<T>, boost::forward_traversal_tag>
  {
    friend class boost::iterator_core_access;

    decltype(auto) increment() noexcept
    {
      return *this = std::shared_ptr<T>::get()->second;
    }

    auto& dereference() const noexcept
    {
      return std::shared_ptr<T>::get()->first;
    }

  public:
    template <typename... Ts>
    constexpr recursive_tuple_iterator(Ts&&... args) noexcept
      : std::shared_ptr<T> {std::forward<Ts>(args)...}
    {}

    using boost::iterator_facade<recursive_tuple_iterator<T>, recursive_tuple_iterator<T>, boost::forward_traversal_tag>::operator*;
    using std::shared_ptr<T>::operator->;

    template <typename U>
    decltype(auto) operator==(U&& rhs) const noexcept
    {
      return static_cast<const std::shared_ptr<T>&>(*this)
          == static_cast<const std::shared_ptr<T>&>(rhs);
    }
  };
} // namespace meevax::utility

#endif // INCLUDED_MEEVAX_UTILITY_RECURSIVE_TUPLE_ITERATOR_HPP

