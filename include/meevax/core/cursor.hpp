#ifndef INCLUDED_MEEVAX_CORE_CURSOR_HPP
#define INCLUDED_MEEVAX_CORE_CURSOR_HPP

#include <meevax/core/accessor.hpp>
#include <meevax/core/pair.hpp>

namespace meevax::core
{
  struct cursor
    : public accessor<pair>,
      public std::iterator<std::input_iterator_tag, cursor>
  {
    template <typename... Ts>
    constexpr cursor(Ts&&... args)
      : accessor<pair> {std::forward<Ts>(args)...}
    {}

    decltype(auto) operator*()
    {
      return access().first;
    }

    decltype(auto) operator->()
    {
      return operator*();
    }

    decltype(auto) operator++()
    {
      return *this = access().second;
    }
  };

  const cursor unit {nullptr};
  const cursor undefined {nullptr};

  inline cursor begin(const accessor<pair>& pair) noexcept
  {
    return pair;
  }

  inline cursor end(const accessor<pair>&) noexcept
  {
    return unit;
  }
} // namespace meevax::core

namespace std
{
  template <typename>
  struct hash;

  template <>
  struct hash<meevax::core::cursor>
    : public std::hash<std::shared_ptr<meevax::core::pair>>
  {};
} // namespace std

#endif // INCLUDED_MEEVAX_CORE_CURSOR_HPP

