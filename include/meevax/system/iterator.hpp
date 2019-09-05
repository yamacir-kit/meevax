#ifndef INCLUDED_MEEVAX_SYSTEM_ITERATOR_HPP
#define INCLUDED_MEEVAX_SYSTEM_ITERATOR_HPP

#include <iterator>

#include <meevax/system/list.hpp>
#include <meevax/system/pair.hpp>

namespace meevax::system
{
  struct iterator
    : public object
  {
    using iterator_category = std::forward_iterator_tag;

    using value_type = object;

    using reference = value_type&;
    using const_reference = const reference;

    using pointer = value_type; // represents homoiconicity

    using difference_type = std::ptrdiff_t;

    using size_type = std::size_t;

    template <typename... Ts>
    constexpr iterator(Ts&&... args)
      : object {std::forward<Ts>(args)...}
    {}

  public: // iterator supports
    decltype(auto) operator*() const
    {
      return car(*this);
    }

    decltype(auto) operator->() const
    {
      return operator*();
    }

    decltype(auto) operator++()
    {
      return *this = cdr(*this);
    }

    decltype(auto) begin() const noexcept
    {
      return *this;
    }

    const iterator end() const noexcept
    {
      return unit;
    }
  };

  iterator begin(const object& object) noexcept
  {
    return object;
  }

  iterator end(const object&) noexcept
  {
    return unit;
  }
} // namespace meevax::system

// namespace std
// {
//   template <typename>
//   struct hash;
//
//   template <>
//   struct hash<meevax::system::iterator>
//     : public std::hash<std::shared_ptr<meevax::system::pair>>
//   {};
// } // namespace std

#endif // INCLUDED_MEEVAX_SYSTEM_ITERATOR_HPP

