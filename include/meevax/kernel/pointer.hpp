#ifndef INCLUDED_MEEVAX_KERNEL_POINTER_HPP
#define INCLUDED_MEEVAX_KERNEL_POINTER_HPP

#include <atomic>
#include <cassert>

#include <meevax/functional/compose.hpp>
#include <meevax/memory/tagged_pointer.hpp>
#include <meevax/type_traits/is_equality_comparable.hpp>
#include <meevax/utility/delay.hpp>
#include <meevax/utility/demangle.hpp>
#include <meevax/utility/module.hpp>
#include <meevax/utility/perfect_forward.hpp>

namespace meevax
{
inline namespace kernel
{
  using null = std::nullptr_t;

  /* ---- Heterogenous Shared Pointer ------------------------------------------
   *
   *  This type requires to the template parameter T inherits top type.
   *
   * ------------------------------------------------------------------------ */
  template <typename T>
  class pointer
    : public std::shared_ptr<T>
  {
    /* ---- Binder -------------------------------------------------------------
     *
     *  The object binder is the actual data pointed to by the pointer type. To
     *  handle all types uniformly, the binder inherits type T and uses dynamic
     *  polymorphism. This provides access to the bound type ID and its
     *  instances. However, the performance is inferior due to the heavy use of
     *  dynamic cast as a price for convenience.
     *
     * ---------------------------------------------------------------------- */
    template <typename B>
    struct binder
      : public virtual T
      , public B
    {
      template <typename... Ts>
      explicit constexpr binder(Ts&&... xs)
        : std::conditional< // Transfers all arguments if Bound Type inherits Top Type virtually.
            std::is_base_of<T, B>::value, T, B
          >::type { std::forward<decltype(xs)>(xs)... }
      {}

      explicit constexpr binder(B&& bound)
        : B { std::forward<decltype(bound)>(bound) }
      {}

      virtual ~binder() = default;

      auto type() const noexcept -> std::type_info const& override
      {
        return typeid(B);
      }

      auto copy() const -> pointer override
      {
        return delay<clone>().yield<pointer>(*this, nullptr);
      }

      auto eqv(pointer const& rhs) const -> bool override
      {
        if constexpr (is_equality_comparable<B>::value)
        {
          if (const auto rhsp { std::dynamic_pointer_cast<B const>(rhs) })
          {
            return static_cast<B const&>(*this) == *rhsp;
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

      auto write_to(std::ostream & port) const -> std::ostream & override
      {
        return delay<write>().yield<decltype(port)>(port, static_cast<B const&>(*this));
      }

      /* ---- Numerical operations ------------------------------------------ */

      #define BOILERPLATE(SYMBOL, RESULT, OPERATION)                           \
      auto operator SYMBOL(pointer const& rhs) const -> RESULT override        \
      {                                                                        \
        return delay<OPERATION>().yield<RESULT>(static_cast<B const&>(*this), rhs); \
      } static_assert(true)

      BOILERPLATE(+, pointer, std::plus<void>);
      BOILERPLATE(-, pointer, std::minus<void>);
      BOILERPLATE(*, pointer, std::multiplies<void>);
      BOILERPLATE(/, pointer, std::divides<void>);
      BOILERPLATE(%, pointer, std::modulus<void>);

      BOILERPLATE(==, bool, std::equal_to<void>);
      BOILERPLATE(!=, bool, std::not_equal_to<void>);
      BOILERPLATE(<,  bool, std::less<void>);
      BOILERPLATE(<=, bool, std::less_equal<void>);
      BOILERPLATE(>,  bool, std::greater<void>);
      BOILERPLATE(>=, bool, std::greater_equal<void>);

      #undef BOILERPLATE
    };

  public:
    pointer(const std::shared_ptr<T>& other)
      : std::shared_ptr<T> { other }
    {}

    template <typename B>
    pointer(const std::shared_ptr<binder<B>>& binding)
      : std::shared_ptr<T> { binding }
    {}

    template <typename... Ts>
    explicit constexpr pointer(Ts&&... xs)
      : std::shared_ptr<T> { std::forward<decltype(xs)>(xs)... }
    {}

    /* ---- Compound Types Binding ------------------------------------------ */

    template <typename Bound, typename... Ts, REQUIRES(std::is_compound<Bound>)>
    static auto bind(Ts&&... xs) -> pointer
    {
      using binding = binder<Bound>;
      return std::make_shared<binding>(std::forward<decltype(xs)>(xs)...);
    }

    /* ---- Immediate Value Binding ----------------------------------------- */

    // template <typename U, typename = typename std::enable_if<is_immediate<U>::value>::type>
    // static pointer bind(U&& value)
    // {
    //   return pointer(box(TODO), [](auto*) {});
    // }

    auto binding() const noexcept -> decltype(auto)
    {
      return std::shared_ptr<T>::operator *();
    }

    auto type() const -> decltype(auto)
    {
      if (*this)
      {
        switch (auto const* data = std::shared_ptr<T>::get(); tag_of(data))
        {
        case 0:
          return (*data).type();

        default:
          return type_of(data);
        }
      }
      else
      {
        return typeid(null);
      }
    }

    /* ---- Type Predicates ------------------------------------------------- */

    template <typename U>
    auto is() const
    {
      return type() == typeid(typename std::decay<U>::type);
    }

    template <typename U,
              typename std::enable_if<
                std::is_null_pointer<typename std::decay<U>::type>::value
              >::type = 0>
    auto is() const
    {
      return not std::shared_ptr<T>::operator bool();
    }

    template <typename U>
    auto is_polymorphically() const
    {
      return std::dynamic_pointer_cast<U>(*this).operator bool();
    }

    /* ---- Accessors ------------------------------------------------------- */

    template <typename U>
    auto as() const -> typename std::add_lvalue_reference<U>::type
    {
      if (auto bound = std::dynamic_pointer_cast<U>(*this); bound)
      {
        return *bound;
      }
      else
      {
        throw make_error("no viable conversion from ", demangle(binding().type()), " to ", demangle(typeid(U)));
      }
    }

    template <typename U,
              typename std::enable_if<is_immediate<U>::value>::type = 0>
    auto as() const -> typename std::decay<U>::type
    {
      // return unbox(std::shared_ptr<T>::get());
      return reinterpret_cast<U>(0);
    }

    decltype(auto) copy() const
    {
      return binding().copy();
    }

    bool eqv(pointer const& rhs) const
    {
      return type() == rhs.type() and binding().eqv(rhs);
    }

    template <typename... Ts>
    decltype(auto) load(Ts&&...)
    {
      return std::atomic_load(this);
    }

    template <typename... Ts>
    decltype(auto) store(Ts&&... xs)
    {
      return std::atomic_store(this, std::forward<decltype(xs)>(xs)...);
    }

    template <typename... Ts>
    decltype(auto) exchange(Ts&&... xs)
    {
      return std::atomic_exchange(this, std::forward<decltype(xs)>(xs)...);
    }
  };

  template <typename T>
  auto operator <<(std::ostream & port, pointer<T> const& rhs) -> decltype(auto)
  {
    return (rhs.template is<null>() ? port << magenta << "()" : rhs.binding().write_to(port)) << reset;
  }

  #define BOILERPLATE(SYMBOL)                                                  \
  template <typename T, typename U>                                            \
  decltype(auto) operator SYMBOL(pointer<T> const& a, pointer<U> const& b)     \
  {                                                                            \
    if (a && b)                                                                \
    {                                                                          \
      return a.binding() SYMBOL b;                                             \
    }                                                                          \
    else                                                                       \
    {                                                                          \
      throw make_error("no viable operation '" #SYMBOL " with ", a, " and ", b); \
    }                                                                          \
  } static_assert(true)

  BOILERPLATE(* );
  BOILERPLATE(+ );
  BOILERPLATE(- );
  BOILERPLATE(/ );
  BOILERPLATE(% );

  BOILERPLATE(< );
  BOILERPLATE(<=);
  BOILERPLATE(> );
  BOILERPLATE(>=);

  #undef BOILERPLATE
} // namespace kernel
} // namespace meevax

namespace std
{
  template <typename T>
  class hash<meevax::kernel::pointer<T>>
    : public hash<std::shared_ptr<T>>
  {};
}

#endif // INCLUDED_MEEVAX_KERNEL_POINTER_HPP
