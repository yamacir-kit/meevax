#ifndef INCLUDED_MEEVAX_KERNEL_OBJECT_HPP
#define INCLUDED_MEEVAX_KERNEL_OBJECT_HPP

#include <meevax/kernel/pointer.hpp>

namespace meevax { inline namespace kernel
{
  /* ---- Identity ---------------------------------------------------------- */

  template <typename T>
  struct alignas(sizeof(word)) identity
  {
    virtual auto type() const noexcept -> const std::type_info&
    {
      return typeid(T);
    }

    virtual auto copy() const -> pointer<T>
    {
      return delay<clone>().yield<pointer<T>>(static_cast<const T&>(*this), nullptr);
    }

    virtual bool eqv(const pointer<T>& rhs) const
    {
      if constexpr (is_equality_comparable<T>::value)
      {
        if (const auto rhsp { std::dynamic_pointer_cast<const T>(rhs) })
        {
          return static_cast<const T&>(*this) == *rhsp;
        }
        else
        {
          return false;
        }
      }
      else
      {
        return false;
      }
    }

    virtual auto write_to(std::ostream& port) const -> decltype(port)
    {
      return delay<write>().yield<decltype(port)>(port, static_cast<const T&>(*this));
    }

    #define BOILERPLATE(SYMBOL, RESULT, OPERATION)                             \
    virtual auto operator SYMBOL(const pointer<T>& rhs) const -> RESULT        \
    {                                                                          \
      return delay<OPERATION>().yield<RESULT>(static_cast<const T&>(*this), rhs); \
    } static_assert(true)

    BOILERPLATE(+, pointer<T>, std::plus<void>);
    BOILERPLATE(-, pointer<T>, std::minus<void>);
    BOILERPLATE(*, pointer<T>, std::multiplies<void>);
    BOILERPLATE(/, pointer<T>, std::divides<void>);
    BOILERPLATE(%, pointer<T>, std::modulus<void>);

    BOILERPLATE(==, bool, std::equal_to<void>);
    BOILERPLATE(!=, bool, std::not_equal_to<void>);
    BOILERPLATE(<,  bool, std::less<void>);
    BOILERPLATE(<=, bool, std::less_equal<void>);
    BOILERPLATE(>,  bool, std::greater<void>);
    BOILERPLATE(>=, bool, std::greater_equal<void>);

    #undef BOILERPLATE
  };

  struct pair; // forward declaration

  using object = pointer<pair>;

  using let = object;

  // TODO Rename to 'cons'?
  using resource = std::allocator<object>;

  template <typename T, typename... Ts>
  inline constexpr decltype(auto) make(Ts&&... xs)
  {
    return object::bind<T>(std::forward<decltype(xs)>(xs)...);
  }

  template <typename T>
  inline constexpr auto make(T&& x)
  {
    return object::bind<typename std::decay<T>::type>(std::forward<decltype(x)>(x));
  }

  // #if __cpp_lib_memory_resource
  // template <typename T,
  //           typename MemoryResource, // XXX (GCC-9 <=)
  //           typename... Ts>
  // inline constexpr decltype(auto) allocate(MemoryResource&& resource, Ts&&... xs)
  // {
  //   return
  //     object::allocate_binding<T>(
  //       std::forward<decltype(resource)>(resource),
  //       std::forward<decltype(xs)>(xs)...);
  // }
  // #endif // __cpp_lib_memory_resource

  static const object unit { nullptr };
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_OBJECT_HPP
