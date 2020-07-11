#ifndef INCLUDED_MEEVAX_KERNEL_POINTER_HPP
#define INCLUDED_MEEVAX_KERNEL_POINTER_HPP

#include <cassert>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <stdexcept> // std::logic_error

#include <meevax/concepts/arithmetic.hpp>
#include <meevax/concepts/is_equality_comparable.hpp>
#include <meevax/concepts/is_stream_insertable.hpp>
#include <meevax/console/escape_sequence.hpp>
#include <meevax/numerical/exact.hpp>
#include <meevax/utility/demangle.hpp>
#include <meevax/utility/hexdump.hpp>
#include <meevax/utility/module.hpp>
#include <meevax/utility/perfect_forward.hpp>
#include <meevax/utility/requires.hpp>

namespace meevax::kernel
{
  /* ==== Linux 64 Bit Address Space ==========================================
  *
  * user   0x0000 0000 0000 0000 ~ 0x0000 7FFF FFFF FFFF
  * kernel 0xFFFF 8000 0000 0000 ~
  *
  *========================================================================= */
  static constexpr auto word_size {sizeof(std::size_t)};

  // The Embeddable Concept is specified for safety.
  template <typename T>
  struct is_embeddable
  {
    using type = typename std::decay<T>::type;

    static constexpr bool value {
      std::is_fundamental<type>::value and sizeof(T) < word_size
    };
  };

  template <typename T>
  using is_not_embeddable
    = std::is_compound<typename std::decay<T>::type>;

  /* ==== Tagged Pointers =====================================================
  *
  */ template <typename T>                                                   /*
  */ using category                                                          /*
  */   = std::integral_constant<                                             /*
  */       std::uintptr_t,                                                   /*
  *
  *               ┌─ The value of meevax::kernel::pointer::get()
  *          ┌────┴─────────┐
  * address   0... .... 0000 => object binder (is 16 byte aligned)
  *
  * boolean   0... 0000 1101 NOTE: sizeof bool is implementation-defined
  *
  * single    0... 0101 1010
  * double    0... 0110 1010
  *
  * int08_t   0... 0011 1000
  * int16_t   0... 0100 1000
  * int32_t   0... 0101 1000
  *
  * uint08_t  0... 0011 1100
  * uint16_t  0... 0100 1100
  * uint32_t  0... 0101 1100
  *                ───┬ ┬┬┬┬
  *                   │ │││└─*/ (std::is_same<bool,     T>::value << 0) |    /*
  *                   │ ││└──*/ (std::is_floating_point<T>::value << 1) |    /*
  *                   │ │└───*/ (std::is_unsigned<      T>::value << 2) |    /*
  *                   │ └────*/ (std::is_arithmetic<    T>::value << 3)      /*
  *                   │
  *                   └────── precision of the type = 2^N bit
  */     >;                                                                  /*
  *
  *========================================================================== */

  constexpr std::uintptr_t category_mask {0x0F};
  constexpr auto           category_mask_width {4};

  template <typename T>
  inline constexpr auto category_of(T const* const value) noexcept
  {
    return reinterpret_cast<std::uintptr_t>(value) bitand category_mask;
  }

  Define_Static_Perfect_Forwarding(is_tagged, category_of);

  template <typename T>
  using precision
    = std::integral_constant<std::uintptr_t, numerical::exact::log2(sizeof(T) * 8)>;

  constexpr std::uintptr_t precision_mask {0xF0};
  constexpr auto           precision_mask_width {4}; // XXX calculate from word size

  template <typename T>
  inline constexpr auto precision_of(T const* const value)
  {
    assert(is_tagged(value));

    return
      (reinterpret_cast<std::uintptr_t>(value) bitand precision_mask)
        >> category_mask_width;
  }

  constexpr std::uintptr_t mask {precision_mask bitor category_mask};
  constexpr auto mask_width {precision_mask_width + category_mask_width};

  template <typename T>
  using tag
    = std::integral_constant<
        std::uintptr_t,
        (precision<T>::value << category_mask_width)
          bitor category<T>::value>;

  // full-tag includes precision.
  template <typename Pointer>
  inline constexpr auto tag_of(Pointer value) noexcept
  {
    assert(is_tagged(value));

    return reinterpret_cast<std::uintptr_t>(value) bitand mask;
  }

  template <typename Pointer>
  inline constexpr auto untagged_value_of(Pointer value) noexcept
  {
    return reinterpret_cast<std::uintptr_t>(value) >> mask_width;
  }

  template <typename T, typename... Ts>
  inline constexpr auto untagged_value_as(Ts&&... xs) noexcept
    -> typename std::decay<T>::type
  {
    auto value {untagged_value_of(
      std::forward<decltype(xs)>(xs)...
    )};
    return reinterpret_cast<typename std::decay<T>::type&>(value);
  }

  /* ==== Heterogenous Shared Pointer =========================================
  *
  * TODO Documentation
  * TODO Rename to 'garbage_collector'
  *
  * This type requires to the template parameter T inherits objective facade.
  *
  *========================================================================= */
  template <typename T>
  class pointer
    : public std::shared_ptr<T>
  {
    /* ==== Binder =============================================================
     *
     * The object binder is the actual data pointed to by the pointer type. To
     * handle all types uniformly, the binder inherits type T and uses dynamic
     * polymorphism. This provides access to the bound type ID and its
     * instances. However, the performance is inferior due to the heavy use of
     * dynamic cast as a price for convenience.
     *
     * ====================================================================== */
    template <typename Bound>
    struct binder
      : public Bound
      , public virtual T
    {
      using top = T;

      using       bound =       Bound;
      using const_bound = const bound;

      using binding = binder<bound>;

      template <typename... Ts>
      explicit constexpr binder(Ts&&... xs)
        : std::conditional< // transfers all arguments if Bound Type inherits Top Type virtually.
            std::is_base_of<top, bound>::value, top, bound
          >::type { std::forward<decltype(xs)>(xs)... }
      {}

      explicit constexpr binder(Bound&& bound)
        : Bound { std::forward<decltype(bound)>(bound) }
      {}

      virtual ~binder() = default;

      auto type() const noexcept -> const std::type_info& override
      {
        return typeid(bound);
      }

    private: // copy
      #if __cpp_if_constexpr

      auto copy() const -> pointer override
      {
        if constexpr (std::is_copy_constructible<binding>::value)
        {
          return std::make_shared<binding>(*this);
        }
        else
        {
          std::stringstream ss {};
          ss << typeid(T).name() << " is not copy_constructible";
          throw std::logic_error { ss.str() };
        }
      }

      #else // __cpp_if_constexpr

      auto copy() const -> pointer override
      {
        return top::template if_copy_constructible<binding>::call_it(*this);
      }

      #endif // __cpp_if_constexpr

    private: // eqv
      #if __cpp_if_constexpr

      auto compare(const pointer& rhs) const -> bool override
      {
        if constexpr (concepts::equality_comparable<bound>::value)
        {
          if (const auto x { std::dynamic_pointer_cast<const bound>(rhs) })
          {
            return static_cast<const bound&>(*this) == *x;
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

      auto compare(const pointer& rhs) const -> bool override
      {
        return top::template if_equality_comparable<bound>::call_it(*this, rhs);
      }

      #endif // __cpp_if_constexpr

    private: // write
      #if __cpp_if_constexpr

      auto write(std::ostream& port) const -> decltype(port) override
      {
        if constexpr (concepts::is_stream_insertable<bound>::value)
        {
          return port << static_cast<const bound&>(*this);
        }
        else
        {
          return port << console::magenta << "#("
                      << console::green << type().name()
                      << console::reset << static_cast<const bound*>(this)
                      << console::magenta << ")"
                      << console::reset;
        }
      }

      #else // __cpp_if_constexpr

      auto write(std::ostream& port) const -> decltype(port) override
      {
        return top::template if_stream_insertable<bound>::call_it(port, *this);
      }

      #endif // __cpp_if_constexpr

    private: // arithmetic
      #if __cpp_if_constexpr

      #define boilerplate(CONCEPT, SYMBOL)                                     \
      auto operator SYMBOL(const pointer& rhs) const -> pointer override       \
      {                                                                        \
        if constexpr (concepts::CONCEPT<bound, decltype(rhs)>::value)          \
        {                                                                      \
          return static_cast<const bound&>(*this) SYMBOL rhs;                  \
        }                                                                      \
        else                                                                   \
        {                                                                      \
          std::stringstream port {};                                           \
          port <<     type().name() << " and "                                 \
               << rhs.type().name() << " are not " #CONCEPT ".";               \
          throw std::runtime_error { port.str() };                             \
        }                                                                      \
      } static_assert(true, "semicolon required after this macro")

      #else // __cpp_if_constexpr

      #define boilerplate(CONCEPT, SYMBOL)                                     \
      template <typename L, typename R, typename = void>                       \
      struct if_##CONCEPT                                                      \
      {                                                                        \
        template <typename... Ts>                                              \
        static auto call(Ts&&...) -> pointer                                   \
        {                                                                      \
          std::stringstream port {};                                           \
          port << typeid(L).name() << " and "                                  \
               << typeid(R).name() << " are not " #CONCEPT ".";                \
          throw std::runtime_error { port.str() };                             \
        }                                                                      \
      };                                                                       \
                                                                               \
      template <typename L, typename R>                                        \
      struct if_##CONCEPT<L, R, typename std::enable_if<concepts::CONCEPT<L, R>::value>::type> \
      {                                                                        \
        static auto call(L&& lhs, R&& rhs) -> pointer                          \
        {                                                                      \
          return lhs SYMBOL rhs;                                               \
        }                                                                      \
      };                                                                       \
                                                                               \
      auto operator SYMBOL(const pointer& rhs) const -> pointer override       \
      {                                                                        \
        return if_##CONCEPT<const bound&, decltype(rhs)>::call(*this, rhs);    \
      } static_assert(true, "semicolon required after this macro")

      #endif // __cpp_if_constexpr

      boilerplate(addable, +);
      boilerplate(divisible, /);
      boilerplate(multipliable, *);
      boilerplate(subtractable, -);

      boilerplate(equality_comparable_with, ==);
      boilerplate(not_equality_comparable_with, !=);

      boilerplate(greater_equal_comparable, >=);
      boilerplate(greater_than_comparable, >);
      boilerplate(less_equal_comparable, <=);
      boilerplate(less_than_comparable, <);

      #undef boilerplate
    };

  public:
    template <typename... Ts>
    constexpr pointer(Ts&&... xs)
      : std::shared_ptr<T> { std::forward<decltype(xs)>(xs)... }
    {}

    /* ==== C/C++ Derived Types Bind ==========================================
    *
    * With this function, you don't have to worry about virtual destructors.
    * std::shared_ptr<T> remembers it has assigned binder type which knows T
    * and the type you binding (both T and Bound's destructor will works
    * correctly).
    *
    *======================================================================== */
    template <typename Bound, typename... Ts, Requires(is_not_embeddable<Bound>)>
    static pointer make_binding(Ts&&... xs)
    {
      using binding = binder<Bound>;

      return
        std::make_shared<binding>(
          std::forward<decltype(xs)>(xs)...);
    }

    template <typename Bound,
              typename MemoryResource, // XXX (GCC-9 <=)
              typename... Ts,
              Requires(is_not_embeddable<Bound>)>
    static pointer allocate_binding(MemoryResource&& resource, Ts&&... xs)
    {
      using binding = binder<Bound>;

      using binding_allocator
        = typename std::decay<
            decltype(resource)
          >::type::template rebind<binding>::other;

      return
        std::allocate_shared<binding>(
          binding_allocator { std::forward<decltype(resource)>(resource) },
          std::forward<decltype(xs)>(xs)...);
    }

    /* ==== C/C++ Primitive Types Bind ========================================
    *
    * TODO: support bind for not is_embeddable types (e.g. double).
    *
    *======================================================================== */
    template <typename U, Requires(is_embeddable<U>)>
    static pointer make_binding(U&& value)
    {
      static auto ignore = [](auto* value)
      {
        std::cerr << "; pointer\t; deleter ignored tagged-pointer (this behavior is intended)" << std::endl;
        std::cerr << ";\t\t; category:\t" << category_of(value) << std::endl;
        std::cerr << ";\t\t; precision:\t" << precision_of(value) << " (" << std::pow(2, precision_of(value)) << "-bits)" << std::endl;
      };

      const auto pattern {*reinterpret_cast<std::uintptr_t*>(&value)};

      return
        pointer(
          reinterpret_cast<T*>(
            pattern << mask_width bitor tag<U>::value),
            ignore);
    }

    decltype(auto) binding() const
    {
      assert(              std::shared_ptr<T>::get() );
      assert(not is_tagged(std::shared_ptr<T>::get()));

      return std::shared_ptr<T>::operator *();
    }

    /* ==== Type Predicates ===================================================
    *
    * TODO: is_compatible_to (non-strict type comparison)
    *
    *======================================================================= */
    decltype(auto) type() const
    {
      switch (auto* value {std::shared_ptr<T>::get()}; category_of(value))
      {
      case category<void*>::value: // address
        return binding().type();

      case category<bool>::value:
        return typeid(bool);

      case category<float>::value:
        switch (precision_of(value))
        {
        case precision<float>::value:
          return typeid(float);

        default:
          throw std::logic_error {"floating-point types with precision greater than 32-bits are not supported."};
        }

      case category<signed int>::value:
        switch (precision_of(value))
        {
        case precision<std::int8_t>::value:
          return typeid(std::int8_t);

        case precision<std::int16_t>::value:
          return typeid(std::int16_t);

        case precision<std::int32_t>::value:
          return typeid(std::int32_t);

        default:
          throw std::logic_error {"signed-integer types with precision greater than 32-bits are not supported."};
        }

      case category<unsigned int>::value:
        switch (precision_of(value))
        {
        case precision<uint8_t>::value:
          return typeid(std::uint8_t);

        case precision<uint16_t>::value:
          return typeid(std::uint16_t);

        case precision<uint32_t>::value:
          return typeid(std::uint32_t);

        default:
          throw std::logic_error {"unsigned-integer types with precision greater than 32-bits are not supported."};
        }

      default:
        throw std::logic_error {"dispatching unimplemented tagged type"};
      }
    }

    template <typename U>
    decltype(auto) is() const
    {
      return type() == typeid(typename std::decay<U>::type);
    }

    /* ==== C/C++ Derived Type Restoration ====================================
    *
    *======================================================================= */
    template <typename U, Requires(is_not_embeddable<U>)>
    U& as() const
    {
      assert(not is_tagged(std::shared_ptr<T>::get()));

      // return dynamic_cast<U&>(binding());
      return *std::dynamic_pointer_cast<U>(*this);
    }

    /* ==== C/C++ Primitive Type Restoration ==================================
    *
    * Currently only supports when the request and actual type match.
    *
    * TODO: Support upcast and downcast of arithmetic types
    *
    *======================================================================= */
    template <typename U, Requires(std::is_arithmetic<U>)>
    auto as() const -> typename std::decay<U>::type
    {
      std::cerr << "; pointer\t; "
                << utility::hexdump<std::uintptr_t>(
                     reinterpret_cast<std::uintptr_t>(std::shared_ptr<T>::get()))
                << std::endl;

      // Helper function "tag_of" includes assertion "is_tagged".
      switch (auto* value {std::shared_ptr<T>::get()}; tag_of(value))
      {
      #define CASE_OF_TYPE(TYPE)                                              \
      case tag<TYPE>::value:                                                  \
        return                                                                \
          static_cast<U>(                                                     \
            untagged_value_as<TYPE>(value))

      CASE_OF_TYPE(bool);

      CASE_OF_TYPE(float);
      CASE_OF_TYPE(double);

      CASE_OF_TYPE(std::int8_t);
      CASE_OF_TYPE(std::int16_t);
      CASE_OF_TYPE(std::int32_t);

      CASE_OF_TYPE(std::uint8_t);
      CASE_OF_TYPE(std::uint16_t);
      CASE_OF_TYPE(std::uint32_t);

      #undef CASE_OF_TYPE

      default:
        throw std::logic_error {"unexpected immediate value restoration"};
      }
    }

    decltype(auto) copy() const
    {
      return binding().copy();
    }

    bool compare(const pointer& rhs) const
    {
      if (type() != rhs.type()) // TODO REMOVE IF OTHER NUMERICAL TYPE IMPLEMENTED
      {
        return false;
      }
      else
      {
        return binding().compare(rhs);
      }
    }

    // NOTE: Can't compile with less than GCC-9 due to a bug in the compiler.
    // Define_Const_Perfect_Forwarding(eqv, compare);

    template <typename... Ts>
    constexpr auto eqv(Ts&&... xs) const -> decltype(auto)
    {
      return
        compare(
          std::forward<decltype(xs)>(xs)...);
    }
  };

  template <typename T>
  decltype(auto) operator<<(std::ostream& os, const pointer<T>& x)
  {
    if (not x)
    {
      return os << console::magenta << "()" << console::reset;
    }
    else
    {
      return x.binding().write(os);
    }
  }

  #define DEFINE_BINARY_OPERATION_DISPATCHER(SYMBOL, NAME)                     \
  template <typename T, typename U>                                            \
  decltype(auto) operator SYMBOL(const pointer<T>& lhs, const pointer<U>& rhs) \
  {                                                                            \
    if (lhs && rhs)                                                            \
    {                                                                          \
      return lhs.binding() SYMBOL rhs;                                         \
    }                                                                          \
    else                                                                       \
    {                                                                          \
      std::stringstream ss {};                                                 \
      ss << "no viable " NAME " with " << lhs << " and " << rhs;               \
      throw std::logic_error { ss.str() };                                     \
    }                                                                          \
  } static_assert(true, "semicolon required after this macro")

  DEFINE_BINARY_OPERATION_DISPATCHER(*, "multiplication");
  DEFINE_BINARY_OPERATION_DISPATCHER(+, "addition");
  DEFINE_BINARY_OPERATION_DISPATCHER(-, "subtraction");
  DEFINE_BINARY_OPERATION_DISPATCHER(/, "division");

  #define DEFINE_COMPARISON_DISPATCHER(SYMBOL)                                 \
  template <typename T, typename U>                                            \
  decltype(auto) operator SYMBOL(const pointer<T>& lhs, const pointer<U>& rhs) \
  {                                                                            \
    if (lhs && rhs)                                                            \
    {                                                                          \
      return lhs.binding() SYMBOL rhs;                                         \
    }                                                                          \
    else                                                                       \
    {                                                                          \
      throw std::logic_error { "" };                                           \
    }                                                                          \
  } static_assert(true, "semicolon required after this macro")

  DEFINE_COMPARISON_DISPATCHER(<);
  DEFINE_COMPARISON_DISPATCHER(<=);
  DEFINE_COMPARISON_DISPATCHER(>);
  DEFINE_COMPARISON_DISPATCHER(>=);
} // namespace meevax::kernel

namespace std
{
  template <typename T>
  class hash<meevax::kernel::pointer<T>>
    : public hash<std::shared_ptr<T>>
  {};
}

#undef DEFINE_BINARY_OPERATION_DISPATCHER
#undef DEFINE_BINARY_OPERATION_FORWARDER
#undef DEFINE_COMPARISON_DISPATCHER
#undef DEFINE_COMPARISON_FORWARDER
#endif // INCLUDED_MEEVAX_KERNEL_POINTER_HPP
