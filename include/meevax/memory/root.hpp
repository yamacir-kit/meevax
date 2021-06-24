#ifndef INCLUDED_MEEVAX_MEMORY_ROOT_HPP
#define INCLUDED_MEEVAX_MEMORY_ROOT_HPP

#include <meevax/memory/collector.hpp>
#include <meevax/memory/simple_pointer.hpp>

namespace meevax
{
inline namespace memory
{
  template <typename T>
  struct root
    : protected collector::root
    , public simple_pointer<T>
  {
    using pointer = typename simple_pointer<T>::pointer;

    explicit constexpr root(std::nullptr_t = nullptr)
      : simple_pointer<T> {}
    {}

    explicit root(pointer const data)
      : simple_pointer<T> { data }
    {
      reset(data);
    }

    explicit root(root const& p)
      : simple_pointer<T> { p.get() }
    {
      reset(p.get());
    }

    template <typename U>
    explicit root(root<U> const& p)
      : simple_pointer<T> { p.get() }
    {
      reset(p.get());
    }

    auto & operator =(root const& another)
    {
      reset(another.get());
      return *this;
    }

    template <typename U>
    auto & operator =(root<U> const& another)
    {
      reset(another.get());
      return *this;
    }

    void reset(pointer const data = nullptr)
    {
      simple_pointer<T>::reset(data);
      collector::root::reset(simple_pointer<T>::get());
    }

    void swap(root & another)
    {
      auto const copy = simple_pointer<T>::get();
      reset(another.get());
      another.reset(copy);
    }
  };
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_ROOT_HPP
