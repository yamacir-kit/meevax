#ifndef INCLUDED_MEEVAX_KERNEL_POINTER_HPP
#define INCLUDED_MEEVAX_KERNEL_POINTER_HPP

#include <cassert>
#include <cmath>
#include <cstdint>
#include <memory> // std::shared_ptr
#include <stdexcept> // std::logic_error
#include <typeinfo> // typeid
#include <utility> // std::forward

#include <meevax/concepts/is_equality_comparable.hpp>
#include <meevax/concepts/is_stream_insertable.hpp>

#include <meevax/kernel/writer.hpp>

#include <meevax/utility/demangle.hpp>
#include <meevax/utility/requires.hpp>

namespace meevax::kernel
{
  template <typename T>
  struct alignas(16) /* category_mask + 1 */ facade // TODO rename to "objective" then move to "object.hpp"
  {
    virtual auto type() const noexcept
      -> const std::type_info&
    {
      return typeid(T);
    }

    virtual std::shared_ptr<T> copy() const
    {
      if constexpr (std::is_copy_constructible<T>::value)
      {
        return std::make_shared<T>(static_cast<const T&>(*this));
      }
      // else throw std::logic_error
      // {
      //   "This is a fatal error report for Meevax core language hackers. The "
      //   "base type of meevax::kernel::pointer requires concept CopyConstructible."
      // };
      else
      {
        static_assert(
          []() { return false; }(),
          "The base type of meevax::kernel::pointer requires concept CopyConstructible.");
      }
    }

    // eqv?
    virtual bool equals(const std::shared_ptr<T>& rhs) const
    {
      if constexpr (concepts::is_equality_comparable<T>::value)
      {
        const auto rhs_ {std::dynamic_pointer_cast<const T>(rhs)};
        assert(rhs_);
        return static_cast<const T&>(*this) == *rhs_;
      }
      else
      {
        return false;
      }
    }

    virtual auto dispatch(std::ostream& os) const
      -> decltype(os)
    {
      return os << static_cast<const T&>(*this);
    };
  };

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
    static constexpr bool value {sizeof(T) < word_size};
  };

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
  *                   │ │││└─*/ (std::is_same<bool,     T>::value << 0) | /*
  *                   │ ││└──*/ (std::is_floating_point<T>::value << 1) | /*
  *                   │ │└───*/ (std::is_unsigned<      T>::value << 2) | /*
  *                   │ └────*/ (std::is_arithmetic<    T>::value << 3)   /*
  *                   │
  *                   └────── precision of the type = 2^N bit
  */     >;                                                                  /*
  *
  *========================================================================= */

  constexpr std::uintptr_t category_mask {0x0F};
  constexpr auto           category_mask_width {4};

  template <typename T>
  constexpr auto category_of(T const* const value)
  {
    return reinterpret_cast<std::uintptr_t>(value) bitand category_mask;
  }

  template <typename T>
  constexpr T log2(const T& k) noexcept
  {
    return (k < 2) ? 0 : 1 + log2(k / 2);
  }

  template <typename T>
  using precision
    = std::integral_constant<std::uintptr_t, log2(sizeof(T) * 8)>;

  constexpr std::uintptr_t precision_mask {0xF0};
  constexpr auto           precision_mask_width {4}; // XXX calculate from word size

  template <typename T>
  constexpr auto precision_of(T const* const value)
  {
    assert(category_of(value) != 0b0000);

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

  template <typename... Ts>
  constexpr bool is_tagged(Ts&&... operands)
  {
    return category_of(std::forward<decltype(operands)>(operands)...);
  }

  template <typename T>
  using is_derived
    = std::negation<
        std::is_scalar<T>>;

  /* ==== Heterogenous Shared Pointer =========================================
  *
  * TODO documentation
  *
  *========================================================================= */
  template <typename T>
  class pointer
    : public std::shared_ptr<T>
  {
    /* ==== Object Binder =====================================================
    *
    * The object binder is the actual data pointed to by the pointer type. To
    * handle all types uniformly, the binder inherits type T and uses dynamic
    * polymorphism. This provides access to the bound type ID and its
    * instances. However, the performance is inferior due to the heavy use of
    * dynamic cast as a price for convenience.
    *
    *======================================================================= */
    template <typename Bound>
    struct /* alignas(mask + 1) */ binder
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
        : Bound {std::forward<Bound>(bound)}
      {}

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

      bool equals(const std::shared_ptr<T>& rhs) const override
      {
        if constexpr (concepts::is_equality_comparable<Bound>::value)
        {
          return static_cast<const Bound&>(*this) == *std::dynamic_pointer_cast<const Bound>(rhs);
        }
        else
        {
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
          return os << highlight::syntax << "#("
                    << highlight::constructor << utility::demangle(typeid(Bound))
                    << attribute::normal << highlight::comment << " #;" << static_cast<const Bound*>(this) << attribute::normal
                    << highlight::syntax << ")"
                    << attribute::normal;
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
    *======================================================================= */
    template <typename Bound, typename... Ts, REQUIRES(is_derived<Bound>)>
    static pointer bind(Ts&&... operands)
    {
      return
        std::make_shared<binder<Bound>>(
          std::forward<decltype(operands)>(operands)...);
    }

    /* ==== C/C++ Primitive Types Bind ========================================
    *
    * TODO: support bind for not is_embeddable types (e.g. double).
    *
    *======================================================================= */
    template <typename U, REQUIRES(is_embeddable<U>)>
    static pointer bind(U&& value)
    {
      static auto ignore = [](auto* value)
      {
        std::cerr << "; pointer-debug\t; deleter ignored tagged-pointer (this behavior is intended)" << std::endl;
        std::cerr << ";\t\t; category:\t" << category_of(value) << std::endl;
        std::cerr << ";\t\t; precision:\t" << precision_of(value) << " (" << std::pow(2, precision_of(value)) << "-bits)" << std::endl;
      };

      return
        std::shared_ptr<T>(
          reinterpret_cast<T*>(
            static_cast<std::uintptr_t>(value) << mask_width bitor tag<U>::value),
            ignore);
    }

    decltype(auto) dereference() const
    {
    #ifndef NDEBUG
      if (*this)
      {
    #endif
        return std::shared_ptr<T>::operator*();
    #ifndef NDEBUG
      }
      else throw std::logic_error
      {
        "meevax::kernel::pointer dererefences nullptr."
      };
    #endif
    }

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

    #define DEFINE_SHORTCUT(NAME) \
    decltype(auto) NAME() const \
    { \
      return dereference().NAME(); \
    }

    DEFINE_SHORTCUT(copy);

    #undef DEFINE_SHORTCUT

    template <typename U>
    decltype(auto) is() const
    {
      return type() == typeid(typename std::decay<U>::type);
    }

    template <typename U, REQUIRES(is_derived<U>)>
    U& as() const
    {
      // The value of derived types are must be ordinal pointer.
      assert(not is_tagged(std::shared_ptr<T>::get()));

      // The given type U and the bound type must match.
      // assert((*this).is<U>());

      return dynamic_cast<U&>(dereference());
    }

    // template <typename U, REQUIRES(is_embeddable<U>)>
    // auto as() const
    //   -> typename std::decay<U>::type
    // {
    //   if (const auto* const value {std::shared_ptr<T>::get()}; is_tagged(value))
    //   {
    //   }
    //   else
    //   {
    //     throw  {__LINE__};
    //   }
    // }

    bool equals(const pointer& rhs) const
    {
      if (type() != rhs.type()) // TODO REMOVE IF OTHER NUMERICAL TYPE IMPLEMENTED
      {
        return false;
      }
      else
      {
        return dereference().equals(rhs);
      }
    }
  };

  template <typename T>
  auto write(const pointer<T>& object, std::ostream& os = std::cout)
    -> decltype(os)
  {
    // write(os) will be dispatched to each type's stream output operator.
    return !object ? (os << highlight::syntax << "()" << attribute::normal) : object.dereference().dispatch(os);
  }

  template <typename T>
  decltype(auto) operator<<(std::ostream& os, const pointer<T>& object)
  {
    return write(object, os);
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

