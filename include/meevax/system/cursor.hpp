#ifndef INCLUDED_MEEVAX_SYSTEM_CURSOR_HPP
#define INCLUDED_MEEVAX_SYSTEM_CURSOR_HPP

#include <meevax/system/accessor.hpp>
#include <meevax/system/pair.hpp>

namespace meevax::system
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
      return std::get<0>(access());
    }

    decltype(auto) operator->()
    {
      return operator*();
    }

    decltype(auto) operator++()
    {
      return *this = std::get<1>(access());
    }
  };

  const cursor unit {nullptr};

  const auto undefined {cursor::bind<std::string>("#<undefined>")};

  inline cursor begin(const accessor<pair>& pair) noexcept
  {
    return pair;
  }

  inline cursor end(const accessor<pair>&) noexcept
  {
    return unit;
  }
} // namespace meevax::system

namespace std
{
  template <typename>
  struct hash;

  template <>
  struct hash<meevax::system::cursor>
    : public std::hash<std::shared_ptr<meevax::system::pair>>
  {};
} // namespace std

#endif // INCLUDED_MEEVAX_SYSTEM_CURSOR_HPP

