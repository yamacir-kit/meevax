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
#include <meevax/type_traits/if_constexpr.hpp>
#include <meevax/utility/demangle.hpp>
#include <meevax/utility/hexdump.hpp>
#include <meevax/utility/module.hpp>
#include <meevax/utility/perfect_forward.hpp>
#include <meevax/utility/requires.hpp>

namespace meevax { inline namespace kernel
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

    private:
      auto copy() const -> pointer override
      {
        return if_is_copy_constructible<binding>::template invoke<pointer>([](auto&&... xs)
        {
          return std::make_shared<binding>(std::forward<decltype(xs)>(xs)...);
        }, *this);
      }

      auto eqv(const pointer& rhs) const -> bool override
      {
        return if_equality_comparable<bound>::template invoke<bool>([](auto&& lhs, auto&& rhs)
        {
          if (const auto rhsp { std::dynamic_pointer_cast<const bound>(rhs) })
          {
            return lhs == *rhsp;
          }
          else
          {
            return false;
          }
        }, static_cast<const bound&>(*this), rhs);
      }

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

    private: // display
      auto display(std::ostream& port) const -> decltype(port) override
      {
        return top::template if_displayable<bound>::call_it(port, *this);
      }

    private: // exact & inexact
      auto exact() const -> bool override
      {
        return top::template if_has_exactness<bound>::call_it(*this);
      }

      auto inexact() const -> bool override
      {
        return top::template if_has_inexactness<bound>::call_it(*this);
      }

    private: // arithmetic
      #define boilerplate(TRAIT, SYMBOL)                                       \
      auto operator SYMBOL(const pointer& rhs) const -> pointer override       \
      {                                                                        \
        return if_##TRAIT<const bound&, decltype(rhs)>::template invoke<pointer>([](auto&& lhs, auto&& rhs) \
        {                                                                      \
          return lhs SYMBOL rhs;                                               \
        }, static_cast<const bound&>(*this), rhs);                             \
      } static_assert(true, "semicolon required after this macro")

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

    union // small-object optimiazation
    {
      bool as_bool;

      char as_char; signed char as_signed_char; unsigned char as_unsigned_char;

      short int as_short_int; unsigned short int as_unsigned_short_int;
      int as_int; unsigned int as_unsigned_int;
      long int as_long_int; unsigned long int as_unsigned_long_int;
      long long int as_long_long_int; unsigned long long int as_unsigned_long_long_int;

      float as_float; double as_double; long double as_long_dougle;
    } aux;

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
    template <typename Bound, typename... Ts,
              typename = typename std::enable_if<is_not_embeddable<Bound>::value>::type>
    static pointer make_binding(Ts&&... xs)
    {
      using binding = binder<Bound>;
      return std::make_shared<binding>(std::forward<decltype(xs)>(xs)...);
    }

    #if __cpp_lib_memory_resource
    template <typename Bound,
              typename MemoryResource, // XXX (GCC-9 <=)
              typename... Ts,
              typename = typename std::enable_if<is_not_embeddable<Bound>::type>::value>
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
    #endif // __cpp_lib_memory_resource

    /* ==== C/C++ Primitive Types Bind ========================================
    *
    * TODO: support bind for not is_embeddable types (e.g. double).
    *
    *======================================================================== */
    template <typename U,
              typename = typename std::enable_if<is_embeddable<U>::value>::type>
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
    template <typename U, typename = typename std::enable_if<is_not_embeddable<U>::value>::type>
    U& as() const
    {
      assert(not is_tagged(std::shared_ptr<T>::get()));

      if (auto bound { std::dynamic_pointer_cast<U>(*this) }; bound)
      {
        return *bound;
      }
      else
      {
        std::stringstream port {};
        port << "type-error: can't treat " << binding() << " as type " << typeid(U).name() << ".";
        throw std::runtime_error { port.str() };
      }
    }

    /* ==== C/C++ Primitive Type Restoration ==================================
    *
    * Currently only supports when the request and actual type match.
    *
    * TODO: Support upcast and downcast of arithmetic types
    *
    *======================================================================= */
    template <typename U,
              typename = typename std::enable_if<std::is_arithmetic<U>::value>::type>
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
        return static_cast<U>(untagged_value_as<TYPE>(value))

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

    bool eqv(const pointer& rhs) const
    {
      return type() != rhs.type() ? false : binding().eqv(rhs);
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

  #define boilerplate(SYMBOL, NAME)                                            \
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

  boilerplate(*, "multiplication");
  boilerplate(+, "addition");
  boilerplate(-, "subtraction");
  boilerplate(/, "division");

  #undef boilerplate

  #define boilerplate(SYMBOL)                                                  \
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

  //     equal_to => eqv or arithmetic_compare
  // not_equal_to => eqv or arithmetic_compare

  boilerplate(<);
  boilerplate(<=);
  boilerplate(>);
  boilerplate(>=);

  #undef boilerplate
}} // namespace meevax::kernel

namespace std
{
  template <typename T>
  class hash<meevax::kernel::pointer<T>>
    : public hash<std::shared_ptr<T>>
  {};
}

#endif // INCLUDED_MEEVAX_KERNEL_POINTER_HPP
