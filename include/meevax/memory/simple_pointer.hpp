#ifndef INCLUDED_MEEVAX_MEMORY_SIMPLE_POINTER_HPP
#define INCLUDED_MEEVAX_MEMORY_SIMPLE_POINTER_HPP

#include <cassert>
#include <cstddef>
#include <memory>
#include <type_traits>

namespace meevax
{
inline namespace memory
{
  template <typename T>
  class simple_pointer
    : public std::pointer_traits<
               typename std::add_pointer<T>::type>
  {
    using typename std::pointer_traits<T *>::pointer;

    pointer data;

    using reference = typename std::add_lvalue_reference<T>::type;

  public:
    explicit constexpr simple_pointer(std::nullptr_t = nullptr)
      : data { nullptr }
    {}

    explicit constexpr simple_pointer(pointer data)
      : data { data }
    {}

    template <typename... Ts>
    explicit constexpr simple_pointer(typename std::pointer_traits<Ts...>::pointer data)
      : data { static_cast<pointer>(data) }
    {}

    explicit constexpr simple_pointer(simple_pointer const& sp)
      : data { sp.get() }
    {}

  public:
    constexpr pointer get() const noexcept
    {
      return data;
    }

    constexpr reference ref() const
    {
      assert(data);
      return *data;
    }

  public:
    decltype(auto) operator =(pointer const& p) noexcept
    {
      data = p.get();
      return *this;
    }

    decltype(auto) operator *() const noexcept
    {
      return ref();
    }

    explicit constexpr operator bool() const noexcept
    {
      return data != nullptr;
    }

  public:
    decltype(auto) load()
    {
      return operator *();
    }

    decltype(auto) store(simple_pointer const& x)
    {
      return data = x.get();
    }
  };

  template <typename T, typename U>
  constexpr auto operator ==(simple_pointer<T> const& x,
                             simple_pointer<U> const& y)
  {
    return x.get() == y.get();
  }

  template <typename T, typename U>
  constexpr auto operator !=(simple_pointer<T> const& x,
                             simple_pointer<U> const& y)
  {
    return x.get() != y.get();
  }
} // namespace memory
} // namespace meevax

namespace std
{
  template <typename T>
  class hash<meevax::memory::simple_pointer<T>>
    : public hash<typename meevax::memory::simple_pointer<T>::pointer>
  {};
}

#endif // INCLUDED_MEEVAX_MEMORY_SIMPLE_POINTER_HPP
