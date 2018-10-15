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

    decltype(auto) operator++() noexcept
    {
      const auto& data {std::shared_ptr<T>::get()};
      return *this = std::get<cdr>(*data);
    }

    decltype(auto) operator*() const noexcept
    {
      const auto& data {std::shared_ptr<T>::get()};
      return std::get<car>(*data);
    }

    // decltype(auto) operator->() const noexcept
    // {
    //   return operator*();
    // }
  };
} // namespace meevax::utility

#endif // INCLUDED_MEEVAX_UTILITY_RECURSIVE_BINARY_TUPLE_ITERATOR_HPP

