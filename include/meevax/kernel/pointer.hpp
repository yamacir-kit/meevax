#ifndef INCLUDED_MEEVAX_KERNEL_POINTER_HPP
#define INCLUDED_MEEVAX_KERNEL_POINTER_HPP

#include <cassert>
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
  struct facade // TODO rename to "objective" then move to "object.hpp"
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

  template <typename T>
  struct is_embeddable
  {
    static constexpr bool value {sizeof(T) < word_size};
  };

  /* ==== Tagged Pointers =====================================================
  *
  */ template <typename T, REQUIRES(is_embeddable<T>)>                       /*
  */ using tag = std::integral_constant<                                     /*
  */               std::uintptr_t,                                           /*
  *
  *                 ┌─ The value of meevax::kernel::pointer::get()
  *          ┌──────┴────┐
  * address   B..... 0000 => points object binder
  * boolean   0....B 1101
  * single    0..011 1010
  * double    0..100 1010
  *
  * int08_t   0..001 1000
  * int16_t   0..010 1000
  * int32_t   0..011 1000
  *
  * uint08_t  0..001 1100
  * uint16_t  0..010 1100
  * uint32_t  0..011 1100
  *              ──┬ ┬┬┬┬
  *                │ │││└─*/ (std::is_same<bool,     T>::value << 0) | /*
  *                │ ││└──*/ (std::is_floating_point<T>::value << 1) | /*
  *                │ │└───*/ (std::is_unsigned<      T>::value << 2) | /*
  *                │ └────*/ (std::is_arithmetic<    T>::value << 3)   /*
  *                │
  *                └────── size = 2^(3 + N) byte
  *
  */              >;                                                          /*
  *
  *========================================================================= */

  constexpr std::uintptr_t mask {0b1111};
  constexpr auto mask_width {4};

  template <typename T>
  constexpr auto masked(T const* const x)
  {
    return reinterpret_cast<std::uintptr_t>(x) bitand mask;
  }

  template <typename... Ts>
  constexpr bool is_tagged(Ts&&... operands)
  {
    return masked(std::forward<decltype(operands)>(operands)...);
  }

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
    struct alignas(mask + 1) binder
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

    // ~pointer()
    // {
    //   if (*this)
    //   {
    //     if (std::shared_ptr<T>::unique())
    //     {
    //       std::cerr << "; pointer\t; deallocating " << *this << std::endl;
    //     }
    //   }
    // }

    /* ========================================================================
    * With this function, you don't have to worry about virtual destructors.
    * std::shared_ptr<T> remembers it has assigned binder type which knows T
    * and the type you binding (both T and Bound's destructor will works
    * correctly).
    *======================================================================= */
    template <typename Bound, typename... Ts,
              REQUIRES(std::negation<std::is_scalar<Bound>>)>
    static pointer bind(Ts&&... operands)
    {
      return
        std::make_shared<binder<Bound>>(
          std::forward<decltype(operands)>(operands)...);
    }

    template <typename U, REQUIRES(is_embeddable<U>)>
    static pointer bind(U&& value)
    {
      return
        std::shared_ptr<T>(
          reinterpret_cast<T*>(
            static_cast<std::uintptr_t>(value) << mask_width bitand tag<U>::value),
          [](auto*)
          {
            std::cerr << "; pointer\t; deleter ignored immediate value" << std::endl;
          });
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
        "This is a fatal error that should be reported to Meevax core language developers (this error only occurs in debug builds). "
        "meevax::kernel::pointer dererefences nullptr."
      };
    #endif
    }

    #define DEFINE_SHORTCUT(NAME) \
    decltype(auto) NAME() const \
    { \
      return dereference().NAME(); \
    }

    DEFINE_SHORTCUT(type);
    DEFINE_SHORTCUT(copy);

    #undef DEFINE_SHORTCUT

    template <typename U>
    decltype(auto) is() const
    {
      return type() == typeid(U);
    }

    template <typename U>
    U& as() const
    {
      // const void* before {&access()};
      // const void* casted {&dynamic_cast<const T&>(access())};
      // std::cerr << "[dynamic_cast] " << before << " => " << casted << " (" << (reinterpret_cast<std::ptrdiff_t>(before) - reinterpret_cast<std::ptrdiff_t>(casted)) << ")" << std::endl;

      return dynamic_cast<U&>(dereference());
    }

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

    static_assert(tag<bool>::value     == 0b1101);

    static_assert(tag<float>::value    == 0b1010);
    // static_assert(tag<double>::value   == 0b1010);

    static_assert(tag<int8_t>::value   == 0b1000);
    static_assert(tag<int16_t>::value  == 0b1000);
    static_assert(tag<int32_t>::value  == 0b1000);
    // static_assert(tag<int64_t>::value  == 0b1000);

    static_assert(tag<uint8_t>::value  == 0b1100);
    static_assert(tag<uint16_t>::value == 0b1100);
    static_assert(tag<uint32_t>::value == 0b1100);
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

