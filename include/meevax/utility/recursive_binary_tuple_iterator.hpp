#ifndef INCLUDED_MEEVAX_UTILITY_RECURSIVE_BINARY_TUPLE_ITERATOR_HPP
#define INCLUDED_MEEVAX_UTILITY_RECURSIVE_BINARY_TUPLE_ITERATOR_HPP

#include <memory>
#include <type_traits>
#include <utility>

#include <boost/iterator/iterator_facade.hpp>

namespace meevax::utility
{
  template <typename T>
  using recursive_forward_iterator_facade
    = boost::iterator_facade<T, T, boost::forward_traversal_tag>;

  template <typename T>
  class recursive_binary_tuple_iterator
    : public std::shared_ptr<T>,
      public recursive_forward_iterator_facade<
               recursive_binary_tuple_iterator<T>
             >
  {
    friend class boost::iterator_core_access;

    static constexpr std::size_t car {0};
    static constexpr std::size_t cdr {1};

    decltype(auto) increment() noexcept
    {
      const auto& data {std::shared_ptr<T>::get()};
      return *this = std::get<cdr>(*data);
    }

    decltype(auto) dereference() const noexcept
    {
      const auto& data {std::shared_ptr<T>::get()};
      return std::get<car>(*data);
    }

  public:
    template <typename... Ts>
    constexpr recursive_binary_tuple_iterator(Ts&&... args) noexcept
      : std::shared_ptr<T> {std::forward<Ts>(args)...}
    {}

    using recursive_forward_iterator_facade<
            recursive_binary_tuple_iterator<T>
          >::operator*;

    using std::shared_ptr<T>::operator->;

    template <typename U>
    decltype(auto) operator==(U&& rhs) const noexcept
    {
      return std::operator==(*this, std::forward<U>(rhs));
    }

    template <typename U>
    decltype(auto) operator!=(U&& rhs) const noexcept
    {
      return not operator==(std::forward<U>(rhs));
    }

    template <auto N>
    decltype(auto) operator[](std::integral_constant<decltype(N), N>) const noexcept
    {
      const auto& data {std::shared_ptr<T>::get()};
      return std::get<N>(*data);
    }
  };
} // namespace meevax::utility

#endif // INCLUDED_MEEVAX_UTILITY_RECURSIVE_BINARY_TUPLE_ITERATOR_HPP

