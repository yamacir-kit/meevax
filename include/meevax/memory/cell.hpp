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
    static inline std::size_t       default_constructor = 0;
    static inline std::size_t       pointer_constructor = 0;

    static inline std::size_t          copy_constructor = 0;
    // static inline std::size_t template_copy_constructor = 0;
    static inline std::size_t          move_constructor = 0;
    // static inline std::size_t template_move_constructor = 0;

    static inline std::size_t          copy_assignment  = 0;
    // static inline std::size_t template_copy_assignment  = 0;
    static inline std::size_t          move_assignment  = 0;
    // static inline std::size_t template_move_assignment  = 0;

    explicit cell(std::nullptr_t = nullptr)
    {
      ++default_constructor;
    }

    explicit cell(simple_pointer<T> const& datum)
      : simple_pointer<T> { datum }
      , collector::object { simple_pointer<T>::get() }
    {
      ++pointer_constructor;
    }

    explicit cell(cell const& datum)
      : simple_pointer<T> { datum.get() }
      , collector::object { simple_pointer<T>::get() }
    {
      ++copy_constructor;
    }

    // template <typename U>
    // explicit cell(cell<U> const& datum)
    //   : simple_pointer<T> { datum.get() }
    //   , collector::object { simple_pointer<T>::get() }
    // {
    //   ++template_copy_constructor;
    // }

    explicit cell(cell && datum)
    {
      ++move_constructor;

      swap(datum);
    }

    // template <typename U>
    // explicit cell(cell<U> && datum)
    // {
    //   ++template_move_constructor;
    //
    //   swap(datum);
    // }

    auto operator =(cell const& another) -> auto &
    {
      ++copy_assignment;

      return store(another);
    }

    // template <typename U>
    // auto operator =(cell<U> const& datum) -> decltype(auto)
    // {
    //   ++template_copy_assignment;
    //
    //   return store(datum);
    // }

    auto operator =(cell && datum) -> auto &
    {
      ++move_assignment;

      swap(datum);

      return *this;
    }

    // template <typename U>
    // auto operator =(cell<U> const& datum)
    // {
    //   ++template_move_assignment;
    //
    //   swap(datum);
    //
    //   return *this;
    // }

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
