#ifndef INCLUDED_MEEVAX_KERNEL_POINTER_HPP
#define INCLUDED_MEEVAX_KERNEL_POINTER_HPP

#include <cassert>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <stdexcept> // std::logic_error

#include <meevax/concepts/is_equality_comparable.hpp>
#include <meevax/concepts/is_stream_insertable.hpp>
#include <meevax/console/escape_sequence.hpp>
#include <meevax/utility/demangle.hpp>
#include <meevax/utility/hexdump.hpp>
#include <meevax/utility/import.hpp>
#include <meevax/utility/requires.hpp>

namespace meevax::kernel
{
  template <typename T>
  inline constexpr T log2(const T& k) noexcept
  {
    return (k < 2) ? 0 : 1 + log2(k / 2);
  }

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
  // template <typename T>
  // struct is_not_embeddable
  // {
  //   using type = typename std::decay<T>::type;
  //
  //   static constexpr bool value {
  //     std::is_compound<type>::value or sizeof(T) < word_size
  //   };
  // };

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

  template <typename... Ts>
  inline constexpr bool is_tagged(Ts&&... operands) noexcept
  {
    return category_of(std::forward<decltype(operands)>(operands)...);
  }

  template <typename T>
  using precision
    = std::integral_constant<std::uintptr_t, log2(sizeof(T) * 8)>;

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
  inline constexpr auto untagged_value_as(Ts&&... operands) noexcept
    -> typename std::decay<T>::type
  {
    auto value {untagged_value_of(
      std::forward<decltype(operands)>(operands)...
    )};
    return reinterpret_cast<typename std::decay<T>::type&>(value);
  }

  /* ==== Heterogenous Shared Pointer =========================================
  *
  * TODO documentation
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
    *======================================================================== */
    template <typename Bound>
    struct binder
      : public Bound
      , public virtual T
    {
      template <typename... Ts>
      explicit constexpr binder(Ts&&... operands)
        : std::conditional< // transfers all arguments if Bound Type inherits Top Type virtually.
            std::is_base_of<T, Bound>::value, T, Bound
          >::type {std::forward<decltype(operands)>(operands)...}
      {}

      explicit constexpr binder(Bound&& bound)
        : Bound {std::forward<decltype(bound)>(bound)}
      {}

      virtual ~binder() = default;

      auto type() const noexcept
        -> const std::type_info& override
      {
        return typeid(Bound);
      }

    private:
      std::shared_ptr<T> copy() const override
      {
        using binding = binder<Bound>;

        if constexpr (std::is_copy_constructible<binding>::value)
        {
          return std::make_shared<binding>(*this);
        }
        else throw std::logic_error
        {
          "The base type of meevax::kernel::pointer requires concept CopyConstructible."
        };
      }

      bool equivalent_to(const std::shared_ptr<T>& rhs) const override
      {
        if constexpr (concepts::is_equality_comparable<Bound>::value)
        {
          return static_cast<const Bound&>(*this) == *std::dynamic_pointer_cast<const Bound>(rhs);
        }
        else
        {
          std::cerr << "; warning\t; equivalence comparison for type "
                    << utility::demangle(type())
                    << " is undefined (always return #false)"
                    << std::endl;
          return false;
        }
      }

      // Override T::dispatch(), then invoke Bound's stream output operator.
      auto dispatch(std::ostream& os) const
        -> decltype(os) override
      {
        if constexpr (concepts::is_stream_insertable<Bound>::value)
        {
          return os << static_cast<const Bound&>(*this);
        }
        else
        {
          return os << console::magenta << "#("
                    << console::green << utility::demangle(typeid(Bound))
                    << console::reset
                    << console::faint << " #;" << static_cast<const Bound*>(this)
                    << console::reset
                    << console::magenta << ")"
                    << console::reset;
        }
      }
    };

  public:
    template <typename... Ts>
    constexpr pointer(Ts&&... operands)
      : std::shared_ptr<T> {std::forward<decltype(operands)>(operands)...}
    {}

    /* ==== Destructor ========================================================
    *
    * TODO: Check all of allocated objects are deallocate correctly.
    *
    *======================================================================= */
    ~pointer()
    {
      if (*this)
      {
        if (std::shared_ptr<T>::unique())
        {
          // std::cerr << "; pointer\t; deallocating " << *this << std::endl;
        }
      }
    }

    /* ==== C/C++ Derived Types Bind ==========================================
    *
    * With this function, you don't have to worry about virtual destructors.
    * std::shared_ptr<T> remembers it has assigned binder type which knows T
    * and the type you binding (both T and Bound's destructor will works
    * correctly).
    *
    *======================================================================== */
    template <typename Bound,
              typename... Ts,
              REQUIRES(is_not_embeddable<Bound>)>
    static pointer make_binding(Ts&&... operands)
    {
      using binding = binder<Bound>;

      return
        std::make_shared<binding>(
          std::forward<decltype(operands)>(operands)...);
    }

    template <typename Bound,
              typename MemoryResource, // XXX (GCC-9 <=)
              typename... Ts,
              REQUIRES(is_not_embeddable<Bound>)>
    static pointer allocate_binding(
      MemoryResource&& resource,
      Ts&&... operands)
    {
      using binding = binder<Bound>;

      using binding_allocator
        = typename std::decay<
            decltype(resource)
          >::type::template rebind<binding>::other;

      // return
      //   std::make_shared<binding>(
      //     std::forward<decltype(operands)>(operands)...);

      return
        std::allocate_shared<binding>(
          binding_allocator {std::forward<decltype(resource)>(resource)},
          std::forward<decltype(operands)>(operands)...);
    }

    /* ==== C/C++ Primitive Types Bind ========================================
    *
    * TODO: support bind for not is_embeddable types (e.g. double).
    *
    *======================================================================== */
    template <typename U, REQUIRES(is_embeddable<U>)>
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
        std::shared_ptr<T>(
          reinterpret_cast<T*>(
            pattern << mask_width bitor tag<U>::value),
            ignore);
    }

    // XXX Need?
    decltype(auto) dereference() const
    {
      assert(*this);
      assert(not is_tagged(std::shared_ptr<T>::get()));

      return std::shared_ptr<T>::operator*();
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
        return dereference().type();

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
    template <typename U, REQUIRES(is_not_embeddable<U>)>
    U& as() const
    {
      assert(not is_tagged(std::shared_ptr<T>::get()));

      // return dynamic_cast<U&>(dereference());
      return *std::dynamic_pointer_cast<U>(*this);
    }

    /* ==== C/C++ Primitive Type Restoration ==================================
    *
    * Currently only supports when the request and actual type match.
    *
    * TODO: Support upcast and downcast of arithmetic types
    *
    *======================================================================= */
    template <typename U, REQUIRES(std::is_arithmetic<U>)>
    auto as() const
      -> typename std::decay<U>::type
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

      CASE_OF_TYPE(std::byte);
      CASE_OF_TYPE(std::int16_t);
      CASE_OF_TYPE(std::int32_t);

      CASE_OF_TYPE(std::byte);
      CASE_OF_TYPE(std::uint16_t);
      CASE_OF_TYPE(std::uint32_t);

      #undef CASE_OF_TYPE

      default:
        throw std::logic_error {"unexpected immediate value restoration"};
      }
    }

    decltype(auto) copy() const
    {
      return dereference().copy();
    }

    bool equivalent_to(const pointer& rhs) const
    {
      if (type() != rhs.type()) // TODO REMOVE IF OTHER NUMERICAL TYPE IMPLEMENTED
      {
        return false;
      }
      else
      {
        return dereference().equivalent_to(rhs);
      }
    }

    template <typename... Ts>
    auto eqv(Ts&&... xs) const
      -> decltype(auto)
    {
      return
        equivalent_to(
          std::forward<decltype(xs)>(xs)...);
    }
  };

  template <typename T>
  decltype(auto) operator<<(std::ostream& os, const pointer<T>& object)
  {
    return not object ? (os << console::magenta << "()" << console::reset)
                      : object.dereference().dispatch(os);
  }

  namespace debug
  {
    // static_assert(tag<void*>::value    == 0b0000);

    static_assert(category<bool>::value == 0b1101);

    static_assert(tag<float>::value    == 0b0101'1010);

    static_assert(tag<int8_t>::value   == 0b0011'1000);
    static_assert(tag<int16_t>::value  == 0b0100'1000);
    static_assert(tag<int32_t>::value  == 0b0101'1000);
    // static_assert(tag<int64_t>::value  == 0b1000);

    static_assert(tag<uint8_t>::value  == 0b0011'1100);
    static_assert(tag<uint16_t>::value == 0b0100'1100);
    static_assert(tag<uint32_t>::value == 0b0101'1100);
    // static_assert(tag<uint64_t>::value == 0b1100);
  } // namespace debug
} // namespace meevax::kernel

namespace std
{
  template <typename T>
  class hash<meevax::kernel::pointer<T>>
    : public hash<std::shared_ptr<T>>
  {};
}

#endif // INCLUDED_MEEVAX_KERNEL_POINTER_HPP

