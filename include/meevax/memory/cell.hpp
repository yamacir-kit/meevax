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
    : public simple_pointer<T>
    , private collector::object
  {
    explicit cell(std::nullptr_t = nullptr)
    {}

    explicit cell(simple_pointer<T> const& datum)
      : simple_pointer<T> { datum }
      , collector::object { simple_pointer<T>::get() }
    {}

    explicit cell(cell const& datum)
      : simple_pointer<T> { datum.get() }
      , collector::object { simple_pointer<T>::get() }
    {}

    template <typename U>
    explicit cell(cell<U> const& datum)
      : simple_pointer<T> { datum.get() }
      , collector::object { simple_pointer<T>::get() }
    {}

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
