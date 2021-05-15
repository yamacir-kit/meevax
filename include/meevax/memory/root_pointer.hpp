#ifndef INCLUDED_MEEVAX_MEMORY_ROOT_POINTER_HPP
#define INCLUDED_MEEVAX_MEMORY_ROOT_POINTER_HPP

#include <meevax/memory/collector.hpp>
#include <meevax/memory/simple_pointer.hpp>

namespace meevax
{
inline namespace memory
{
  template <typename T>
  struct root_pointer
    : protected collector::collectable
    , public simple_pointer<T>
  {
    using pointer = typename simple_pointer<T>::pointer;

  public: /* ---- CONSTRUCTORS ---------------------------------------------- */

    explicit root_pointer(std::nullptr_t = nullptr)
      : simple_pointer<T> {}
    {}

    explicit root_pointer(pointer const data)
      : simple_pointer<T> { data }
    {
      reset(data);
    }

    explicit root_pointer(root_pointer const& p)
      : simple_pointer<T> { p.get() }
    {
      reset(p.get());
    }

    template <typename U>
    explicit root_pointer(root_pointer<U> const& p)
      : simple_pointer<T> { p.get() }
    {
      reset(p.get());
    }

  public: /* ---- ACCESSORS ------------------------------------------------- */

    void reset(pointer const p = nullptr)
    {
      simple_pointer<T>::reset(p);
      collectable::reset(simple_pointer<T>::get());
    }

  public: /* ---- OPERATOR OVERLOADS ---------------------------------------- */

    auto & operator =(root_pointer const& p)
    {
      reset(p.get());
      return *this;
    }

    template <typename U>
    auto & operator =(root_pointer<U> const& p)
    {
      reset(p.get());
      return *this;
    }

    void swap(root_pointer & other)
    {
      auto const copy = simple_pointer<T>::get();
      reset(other.get());
      other.reset(copy);
    }
  };
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_ROOT_POINTER_HPP
