#ifndef INCLUDED_MEEVAX_KERNEL_OBJECT_HPP
#define INCLUDED_MEEVAX_KERNEL_OBJECT_HPP

#include <meevax/kernel/pointer.hpp>

namespace meevax { inline namespace kernel
{
  /* ==== Identity =============================================================
   *
   * ======================================================================== */
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
      return if_stream_insertable<T>::call_it(port, static_cast<const T&>(*this));
    }

  public: // arithmetic
    // override by binder's operators
    #define BOILERPLATE(SYMBOL)                                                \
    virtual auto operator SYMBOL(const pointer<T>&) const -> pointer<T>        \
    {                                                                          \
      std::stringstream ss {};                                                 \
      ss << __FILE__ << ":" << __LINE__;                                       \
      throw std::logic_error { ss.str() };                                     \
    } static_assert(true, "semicolon required after this macro")

    BOILERPLATE(*);
    BOILERPLATE(+);
    BOILERPLATE(-);
    BOILERPLATE(/);

    #undef BOILERPLATE

    #define BOILERPLATE(SYMBOL)                                                \
    virtual auto operator SYMBOL(const pointer<T>&) const -> bool              \
    {                                                                          \
      std::stringstream ss {};                                                 \
      ss << __FILE__ << ":" << __LINE__;                                       \
      throw std::logic_error { ss.str() };                                     \
    } static_assert(true)

    BOILERPLATE(==);
    BOILERPLATE(!=);
    BOILERPLATE(<);
    BOILERPLATE(<=);
    BOILERPLATE(>);
    BOILERPLATE(>=);

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
