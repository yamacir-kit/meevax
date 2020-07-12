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

  public: // copy
    #if __cpp_if_constexpr

    virtual auto copy() const -> pointer<T>
    {
      if constexpr (std::is_copy_constructible<T>::value)
      {
        return std::make_shared<T>(static_cast<const T&>(*this));
      }
      else
      {
        std::stringstream port {};
        port << typeid(T).name() << " is not copy_constructible";
        throw std::logic_error { port.str() };
      }
    }

    #else // __cpp_if_constexpr

    template <typename U, typename = void>
    struct if_copy_constructible
    {
      template <typename... Ts>
      static auto call_it(Ts&&...) -> pointer<T>
      {
        std::stringstream port {};
        port << typeid(U).name() << " is not copy_constructible";
        throw std::logic_error { port.str() };
      }
    };

    template <typename U>
    struct if_copy_constructible<U, typename std::enable_if<std::is_copy_constructible<U>::value>::type>
    {
      template <typename... Ts>
      static auto call_it(Ts&&... xs) -> pointer<T>
      {
        return std::make_shared<U>(std::forward<decltype(xs)>(xs)...);
      }
    };

    virtual auto copy() const -> pointer<T>
    {
      return if_copy_constructible<T>::call_it(static_cast<const T&>(*this));
    }

    #endif // __cpp_if_constexpr

  public: // eqv
    #if __cpp_if_constexpr

    virtual bool eqv(const pointer<T>& rhs) const
    {
      if constexpr (concepts::equality_comparable<T>::value)
      {
        if (const auto x { std::dynamic_pointer_cast<const T>(rhs) })
        {
          return static_cast<const T&>(*this) == *x;
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

    #else // __cpp_if_constexpr

    template <typename U, typename = void>
    struct if_equality_comparable
    {
      template <typename... Ts>
      static auto call_it(Ts&&...) -> bool
      {
        return false;
      }
    };

    template <typename U>
    struct if_equality_comparable<U, typename std::enable_if<concepts::equality_comparable<U>::value>::type>
    {
      static auto call_it(const U& lhs, const pointer<T>& rhs) -> bool
      {
        if (const auto rhs_ { std::dynamic_pointer_cast<const U>(rhs) })
        {
          return lhs == *rhs_;
        }
        else
        {
          return false;
        }
      }
    };

    virtual bool eqv(const pointer<T>& rhs) const
    {
      return if_equality_comparable<T>::call_it(static_cast<const T&>(*this), rhs);
    }

    #endif // __cpp_if_constexpr

  public: // write
    #if __cpp_if_constexpr

    virtual auto write(std::ostream& port) const -> decltype(port)
    {
      if constexpr (concepts::is_stream_insertable<T>::value)
      {
        return port << static_cast<const T&>(*this);
      }
      else
      {
        return port << console::magenta << "#,("
                    << console::green << type().name()
                    << console::reset << " " << static_cast<const T*>(this)
                    << console::magenta << ")"
                    << console::reset;
      }
    };

    #else // __cpp_if_constexpr

    template <typename U, typename = void>
    struct if_stream_insertable
    {
      static auto call_it(std::ostream& port, const U& rhs) -> decltype(port)
      {
        return port << console::magenta << "#,("
                    << console::green << typeid(U).name()
                    << console::reset << " " << static_cast<const U*>(&rhs)
                    << console::magenta << ")"
                    << console::reset;
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

    #endif // __cpp_if_constexpr

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

  public: // arithmetic
    // override by binder's operators
    #define boilerplate(SYMBOL)                                                \
    virtual auto operator SYMBOL(const pointer<T>&) const -> pointer<T>        \
    {                                                                          \
      std::stringstream ss {};                                                 \
      ss << __FILE__ << ":" << __LINE__;                                       \
      throw std::logic_error { ss.str() };                                     \
    } static_assert(true, "semicolon required after this macro")

    boilerplate(*);
    boilerplate(+);
    boilerplate(-);
    boilerplate(/);

    boilerplate(==);
    boilerplate(!=);

    boilerplate(<);
    boilerplate(<=);
    boilerplate(>);
    boilerplate(>=);

    #undef boilerplate
  };

  struct pair; // forward declaration

  using object = pointer<pair>;

  // TODO Rename to 'cons'?
  using resource = std::allocator<object>;

  template <typename T, typename... Ts>
  inline constexpr decltype(auto) make(Ts&&... xs)
  {
    return object::make_binding<T>(std::forward<decltype(xs)>(xs)...);
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

  #define boilerplate(TYPENAME)                                                \
  struct TYPENAME##_t                                                          \
  {                                                                            \
    TYPENAME##_t() = default;                                                  \
                                                                               \
    friend auto operator<<(std::ostream& os, const TYPENAME##_t&)              \
      -> decltype(os)                                                          \
    {                                                                          \
      return os << console::faint << "#;" #TYPENAME << console::reset;         \
    }                                                                          \
  };                                                                           \
                                                                               \
  static const auto TYPENAME { make<TYPENAME##_t>() }

  boilerplate(undefined);
  boilerplate(unspecified);

  #undef boilerplate
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_OBJECT_HPP
