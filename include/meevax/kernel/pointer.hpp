#ifndef INCLUDED_MEEVAX_KERNEL_POINTER_HPP
#define INCLUDED_MEEVAX_KERNEL_POINTER_HPP

#include <meevax/functional/compose.hpp>
#include <meevax/memory/root_pointer.hpp>
#include <meevax/type_traits/is_equality_comparable.hpp>
#include <meevax/utility/delay.hpp>
#include <meevax/utility/module.hpp>

namespace meevax
{
inline namespace kernel
{
  template <template <typename...> typename Pointer, typename T>
  class heterogeneous : public Pointer<T>
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
        : std::conditional<std::is_base_of<T, B>::value, T, B>::type { std::forward<decltype(xs)>(xs)... }
      {}

      ~binder() override = default;

      auto type() const noexcept -> std::type_info const& override
      {
        return typeid(B);
      }

      bool eqv(heterogeneous const& x) const override
      {
        if constexpr (is_equality_comparable<B>::value)
        {
          auto const* p = dynamic_cast<B const*>(x.get());

          return p and *p == static_cast<B const&>(*this);
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

      #define BOILERPLATE(SYMBOL, RESULT, FUNCTOR)                             \
      auto operator SYMBOL(heterogeneous const& x) const -> RESULT override    \
      {                                                                        \
        return delay<FUNCTOR>().yield<RESULT>(static_cast<B const&>(*this), x); \
      } static_assert(true)

      BOILERPLATE(+, heterogeneous, std::plus<void>);
      BOILERPLATE(-, heterogeneous, std::minus<void>);
      BOILERPLATE(*, heterogeneous, std::multiplies<void>);
      BOILERPLATE(/, heterogeneous, std::divides<void>);
      BOILERPLATE(%, heterogeneous, std::modulus<void>);

      BOILERPLATE(==, bool, std::equal_to<void>);
      BOILERPLATE(!=, bool, std::not_equal_to<void>);
      BOILERPLATE(<,  bool, std::less<void>);
      BOILERPLATE(<=, bool, std::less_equal<void>);
      BOILERPLATE(>,  bool, std::greater<void>);
      BOILERPLATE(>=, bool, std::greater_equal<void>);

      #undef BOILERPLATE
    };

  public: /* ---- CONSTRUCTORS ---------------------------------------------- */

    using Pointer<T>::Pointer;

    template <typename B, typename... Ts, REQUIRES(std::is_compound<B>)>
    static auto allocate(Ts&&... xs)
    {
      if constexpr (std::is_same<B, T>::value)
      {
        return static_cast<heterogeneous>(new (gc) T(std::forward<decltype(xs)>(xs)...));
      }
      else
      {
        return static_cast<heterogeneous>(new (gc) binder<B>(std::forward<decltype(xs)>(xs)...));
      }
    }

  public: /* ---- TYPE PREDICATES ------------------------------------------- */

    auto type() const -> decltype(auto)
    {
      return *this ? Pointer<T>::load().type() : typeid(null);
    }

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
      return not static_cast<bool>(*this);
    }

    template <typename U>
    auto is_polymorphically() const
    {
      return dynamic_cast<U const*>(Pointer<T>::get()) != nullptr;
    }

  public: /* ---- ACCESSORS ------------------------------------------------- */

    template <typename U>
    auto as() const -> typename std::add_lvalue_reference<U>::type
    {
      if (auto * p = dynamic_cast<U *>(Pointer<T>::get()); p)
      {
        return *p;
      }
      else
      {
        throw make_error(
          "no viable conversion from ", demangle(Pointer<T>::load().type()), " to ", demangle(typeid(U)));
      }
    }

    bool eqv(heterogeneous const& rhs) const
    {
      return type() == rhs.type() and Pointer<T>::load().eqv(rhs);
    }
  };

  template <template <typename...> typename Pointer, typename T>
  auto operator <<(output_port & port, heterogeneous<Pointer, T> const& datum) -> output_port &
  {
    return (datum.template is<null>() ? port << magenta << "()" : datum.load().write_to(port)) << reset;
  }

  #define BOILERPLATE(SYMBOL)                                                  \
  template <template <typename...> typename Pointer, typename T>               \
  auto operator SYMBOL(heterogeneous<Pointer, T> const& a,                     \
                       heterogeneous<Pointer, T> const& b) -> decltype(auto)   \
  {                                                                            \
    if (a and b)                                                               \
    {                                                                          \
      return a.load() SYMBOL b;                                                \
    }                                                                          \
    else                                                                       \
    {                                                                          \
      throw make_error("no viable operation " #SYMBOL " with ", a, " and ", b); \
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

#endif // INCLUDED_MEEVAX_KERNEL_POINTER_HPP
