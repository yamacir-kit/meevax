#ifndef INCLUDED_MEEVAX_KERNEL_POINTER_HPP
#define INCLUDED_MEEVAX_KERNEL_POINTER_HPP

#include <cassert>
#include <memory> // std::shared_ptr
#include <stdexcept> // std::logic_error
#include <typeinfo> // typeid
#include <utility> // std::forward

#include <meevax/concepts/is_equality_comparable.hpp>
#include <meevax/concepts/is_stream_insertable.hpp>

#include <meevax/kernel/writer.hpp>

#include <meevax/utility/demangle.hpp>

namespace meevax::kernel
{
  template <typename T>
  struct facade
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
      else throw std::logic_error
      {
        "This is a fatal error for Meevax core language hackers. "
        "The concept CopyConstructible is required for the base type of Meevax::kernel::pointer."
      };
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

  /**
   * Reference counting garbage collector built-in heteropointer.
   */
  template <typename T>
  class pointer
    : public std::shared_ptr<T>
  {
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
          "This is a fatal error for Meevax library developers. "
          "The bound type of meevax::kernel::pointer is required the concept CopyConstructible."
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

    /**
     * With this function, you don't have to worry about virtual destructors.
     * `std::shared_ptr<T>` remembers it has assigned binder type which knows T
     * and the type you binding (both `T` and `Bound`'s destructor will works
     * correctly).
     */
    template <typename Bound, typename... Ts>
    static constexpr pointer<T> bind(Ts&&... operands)
    {
      using binding = binder<Bound>;
      return std::make_shared<binding>(std::forward<decltype(operands)>(operands)...);
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

    #define SHORT_ACCESS(NAME) \
    decltype(auto) NAME() const \
    { \
      return dereference().NAME(); \
    }

    SHORT_ACCESS(type);
    SHORT_ACCESS(copy);

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
} // namespace meevax::kernel

namespace std
{
  template <typename T>
  class hash<meevax::kernel::pointer<T>>
    : public hash<std::shared_ptr<T>>
  {};
}

#endif // INCLUDED_MEEVAX_KERNEL_POINTER_HPP

