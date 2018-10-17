#ifndef INCLUDED_MEEVAX_UTILITY_RECURSIVE_BINARY_TUPLE_ITERATOR_HPP
#define INCLUDED_MEEVAX_UTILITY_RECURSIVE_BINARY_TUPLE_ITERATOR_HPP

#include <iterator>
#include <memory>
#include <type_traits>
#include <utility>

namespace meevax::utility
{
  template <typename T>
  class recursive_binary_tuple_iterator
    : public std::shared_ptr<T>
  {
    static constexpr std::size_t car {0};
    static constexpr std::size_t cdr {1};

  public:
    using iterator_category = std::input_iterator_tag;
    using difference_type = std::ptrdiff_t;
    using value_type = recursive_binary_tuple_iterator<T>;
    using reference = value_type&;
    using pointer = value_type;

  public:
    template <typename... Ts>
    constexpr recursive_binary_tuple_iterator(Ts&&... args) noexcept
      : std::shared_ptr<T> {std::forward<Ts>(args)...}
    {}

    decltype(auto) access() const noexcept
    {
      const auto& data {std::shared_ptr<T>::get()};
      return *data;
    }

    decltype(auto) operator++() noexcept
    {
      return *this = std::get<cdr>(access());
    }

    decltype(auto) operator*() const noexcept
    {
      return std::get<car>(access());
    }

    decltype(auto) operator->() const noexcept
    {
      return operator*();
    }

  public:
    template <typename U>
    friend decltype(auto) car(U&& pair)
    {
      return *pair;
    }

    template <typename U>
    friend decltype(auto) cdr(U&& pair)
    {
      return std::get<cdr>(pair.access());
    }
  };
} // namespace meevax::utility

#define cadar(e) car(cdr(car(e)))
#define caddar(e) car(cdr(cdr(car(e))))

#define cadr(e) car(cdr(e))
#define caddr(e) car(cdr(cdr(e)))
#define cadddr(e) car(cdr(cdr(cdr(e))))

#endif // INCLUDED_MEEVAX_UTILITY_RECURSIVE_BINARY_TUPLE_ITERATOR_HPP

