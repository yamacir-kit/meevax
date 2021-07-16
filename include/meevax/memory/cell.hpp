#ifndef INCLUDED_MEEVAX_MEMORY_CELL_HPP
#define INCLUDED_MEEVAX_MEMORY_CELL_HPP

#include <meevax/memory/collector.hpp>
#include <meevax/memory/simple_pointer.hpp>

namespace meevax
{
inline namespace memory
{
  template <typename T>
  struct cell
    : protected collector::object
    , public simple_pointer<T>
  {
    explicit cell(simple_pointer<T> const p = nullptr)
      : simple_pointer<T> {}
    {
      reset(p.get());
    }

    explicit cell(cell const& p)
      : simple_pointer<T> {}
    {
      reset(p.get());
    }

    template <typename U>
    explicit cell(cell<U> const& p)
      : simple_pointer<T> {}
    {
      reset(p.get());
    }

    auto operator =(cell const& another) -> auto &
    {
      return store(another);
    }

    template <typename... Ts>
    auto operator =(Ts&&... xs) -> decltype(auto)
    {
      return store(std::forward<decltype(xs)>(xs)...);
    }

    void reset(const_pointer<T> data = nullptr)
    {
      collector::object::reset(simple_pointer<T>::reset(data));
    }

    auto store(cell const& another) -> auto &
    {
      reset(another.get());
      return *this;
    }

    void swap(cell & another)
    {
      auto const copy = simple_pointer<T>::get();
      reset(another.get());
      another.reset(copy);
    }
  };
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_CELL_HPP
