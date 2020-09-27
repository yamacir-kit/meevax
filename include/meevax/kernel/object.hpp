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
        return std::make_shared<T>(std::forward<decltype(xs)>(xs)...);
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

  public: // write
    template <typename U, typename = void>
    struct if_stream_insertable
    {
      static auto call_it(std::ostream& port, const U& rhs) -> decltype(port)
      {
        return port << magenta << "#,("
                    << green << typeid(U).name()
                    << reset << " " << static_cast<const U*>(&rhs)
                    << magenta << ")"
                    << reset;
      }
    };

    template <typename U>
    struct if_stream_insertable<U, typename std::enable_if<concepts::is_stream_insertable<U>::value>::type>
    {
      static auto call_it(std::ostream& port, const U& rhs) -> decltype(port)
      {
        return port << rhs;
      }
    };

    virtual auto write(std::ostream& port) const -> decltype(port)
    {
      return if_stream_insertable<T>::call_it(port, static_cast<const T&>(*this));
    }

  public: // display
    template <typename U, typename = void>
    struct if_displayable
    {
      static auto call_it(std::ostream& port, const U& target) -> decltype(port)
      {
        return port << target;
      }
    };

    template <typename U>
    struct if_displayable<U, type_traits::void_t<decltype(
             std::declval<U>().write_string(
               std::declval<std::ostream&>()))>>
    {
      static auto call_it(std::ostream& port, const U& target) -> decltype(port)
      {
        return target.write_string(port);
      }
    };

    virtual auto display(std::ostream& port) const -> decltype(port)
    {
      return if_displayable<T>::call_it(port, static_cast<const T&>(*this));
    }

  public: // exact & inexact
    template <typename Top, typename = void>
    struct if_has_exactness
    {
      static auto call_it(const Top&)
      {
        return false;
      }
    };

    template <typename Top>
    struct if_has_exactness<Top, type_traits::void_t<decltype(std::declval<Top>().exact())>>
    {
      static auto call_it(const Top& top) -> decltype(auto)
      {
        return top.exact();
      }
    };

    virtual auto exact() const -> bool
    {
      return if_has_exactness<T>::call_it(static_cast<const T&>(*this));
    }

    template <typename Top, typename = void>
    struct if_has_inexactness
    {
      static auto call_it(const Top&)
      {
        return false;
      }
    };

    template <typename Top>
    struct if_has_inexactness<Top, type_traits::void_t<decltype(std::declval<Top>().inexact())>>
    {
      static auto call_it(const Top& top) -> decltype(auto)
      {
        return top.inexact();
      }
    };

    virtual auto inexact() const -> bool
    {
      return if_has_exactness<T>::call_it(static_cast<const T&>(*this));
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
