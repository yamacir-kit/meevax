#ifndef INCLUDED_MEEVAX_KERNEL_OBJECT_HPP
#define INCLUDED_MEEVAX_KERNEL_OBJECT_HPP

#include <meevax/kernel/pointer.hpp>

namespace meevax { inline namespace kernel
{
  /* ---- Identity ---------------------------------------------------------- */

  template <typename T>
  struct alignas(category_mask + 1) identity
  {
    virtual auto type() const noexcept -> const std::type_info&
    {
      return typeid(T);
    }

    virtual auto copy() const -> pointer<T>
    {
      return if_is_copy_constructible<T>::template invoke<pointer<T>>([](auto&&... xs)
      {
        return static_cast<pointer<T>>(std::make_shared<T>(std::forward<decltype(xs)>(xs)...));
      }, static_cast<const T&>(*this));
    }

    virtual bool eqv(const pointer<T>& rhs) const
    {
      return if_equality_comparable<T>::template invoke<bool>([](auto&& lhs, auto&& rhs)
      {
        if (const auto rhsp { std::dynamic_pointer_cast<const T>(rhs) })
        {
          return lhs == *rhsp;
        }
        else
        {
          return false;
        }
      }, static_cast<const T&>(*this), rhs);
    }

    virtual auto write(std::ostream& port) const -> decltype(port)
    {
      // TODO
      //
      // if_<T, is_stream_insertable>::operator <<();

      return if_stream_insertable<T>::call_it(port, static_cast<const T&>(*this));
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

  #if __cpp_lib_memory_resource
  template <typename T,
            typename MemoryResource, // XXX (GCC-9 <=)
            typename... Ts>
  inline constexpr decltype(auto) allocate(MemoryResource&& resource, Ts&&... xs)
  {
    return
      object::allocate_binding<T>(
        std::forward<decltype(resource)>(resource),
        std::forward<decltype(xs)>(xs)...);
  }
  #endif // __cpp_lib_memory_resource

  static const object unit {nullptr};

  // Ghost types
  #define BOILERPLATE(TYPENAME)                                                \
  struct TYPENAME##_t                                                          \
  {                                                                            \
    TYPENAME##_t() = default;                                                  \
                                                                               \
    friend auto operator <<(std::ostream& os, const TYPENAME##_t&) -> decltype(os) \
    {                                                                          \
      return os << faint << "#;" #TYPENAME << reset;                           \
    }                                                                          \
  };                                                                           \
                                                                               \
  static const auto TYPENAME { make<TYPENAME##_t>() }

  BOILERPLATE(undefined);
  BOILERPLATE(unspecified);

  #undef BOILERPLATE
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_OBJECT_HPP
