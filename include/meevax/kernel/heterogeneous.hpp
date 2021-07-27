#ifndef INCLUDED_MEEVAX_KERNEL_POINTER_HPP
#define INCLUDED_MEEVAX_KERNEL_POINTER_HPP

#include <meevax/functional/compose.hpp>
#include <meevax/memory/cell.hpp>
#include <meevax/type_traits/is_equality_comparable.hpp>
#include <meevax/utility/delay.hpp>
#include <meevax/utility/module.hpp>

namespace meevax
{
inline namespace kernel
{
  template <template <typename...> typename Pointer, typename Top>
  class heterogeneous : public Pointer<Top>
  {
    template <typename Bound>
    struct binder
      : public virtual Top
      , public Bound
    {
      template <typename... Ts>
      explicit constexpr binder(Ts&&... xs)
        : std::conditional<std::is_base_of<Top, Bound>::value, Top, Bound>::type { std::forward<decltype(xs)>(xs)... }
      {}

      ~binder() override = default;

      auto type() const noexcept -> std::type_info const& override
      {
        return typeid(Bound);
      }

      bool eqv(heterogeneous const& x) const override
      {
        if constexpr (is_equality_comparable<Bound>::value)
        {
          if (auto const* address = dynamic_cast<Bound const*>(x.get()); address)
          {
            return *address == static_cast<Bound const&>(*this);
          }
          else
          {
            return std::is_same<Bound, std::nullptr_t>::value;
          }
        }
        else
        {
          return false;
        }
      }

      auto write_to(std::ostream & port) const -> std::ostream & override
      {
        return delay<write>().yield<decltype(port)>(port, static_cast<Bound const&>(*this));
      }

      #define BOILERPLATE(SYMBOL, RESULT, FUNCTION)                            \
      auto operator SYMBOL(heterogeneous const& x) const -> RESULT override    \
      {                                                                        \
        return delay<FUNCTION>().yield<RESULT>(static_cast<Bound const&>(*this), x); \
      } static_assert(true)

      BOILERPLATE(+, heterogeneous, std::plus      <void>);
      BOILERPLATE(-, heterogeneous, std::minus     <void>);
      BOILERPLATE(*, heterogeneous, std::multiplies<void>);
      BOILERPLATE(/, heterogeneous, std::divides   <void>);
      BOILERPLATE(%, heterogeneous, std::modulus   <void>);

      BOILERPLATE(==, bool, std::equal_to     <void>);
      BOILERPLATE(!=, bool, std::not_equal_to <void>);
      BOILERPLATE(<,  bool, std::less         <void>);
      BOILERPLATE(<=, bool, std::less_equal   <void>);
      BOILERPLATE(>,  bool, std::greater      <void>);
      BOILERPLATE(>=, bool, std::greater_equal<void>);

      #undef BOILERPLATE
    };

  public: /* ---- CONSTRUCTORS ---------------------------------------------- */

    using Pointer<Top>::Pointer;

    template <typename Bound, typename... Ts, REQUIRES(std::is_compound<Bound>)>
    static auto allocate(Ts&&... xs)
    {
      if constexpr (std::is_same<Bound, Top>::value)
      {
        return static_cast<heterogeneous>(new (gc) Top(std::forward<decltype(xs)>(xs)...));
      }
      else
      {
        return static_cast<heterogeneous>(new (gc) binder<Bound>(std::forward<decltype(xs)>(xs)...));
      }
    }

  public: /* ---- TYPE PREDICATES ------------------------------------------- */

    auto type() const -> decltype(auto)
    {
      return *this ? Pointer<Top>::load().type() : typeid(null);
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
      return dynamic_cast<pointer<const U>>(Pointer<Top>::get()) != nullptr;
    }

  public: /* ---- ACCESSORS ------------------------------------------------- */

    template <typename U>
    auto as() const -> typename std::add_lvalue_reference<U>::type
    {
      if (auto * const address = dynamic_cast<U *>(Pointer<Top>::get()); address)
      {
        return *address;
      }
      else
      {
        throw make_error(
          "no viable conversion from ", demangle(Pointer<Top>::load().type()), " to ", demangle(typeid(U)));
      }
    }

    bool eqv(heterogeneous const& rhs) const
    {
      return type() == rhs.type() and Pointer<Top>::load().eqv(rhs);
    }
  };

  template <template <typename...> typename Pointer, typename Top>
  auto operator <<(std::ostream & port, heterogeneous<Pointer, Top> const& datum) -> std::ostream &
  {
    return (datum.template is<null>() ? port << magenta << "()" : datum.load().write_to(port)) << reset;
  }

  #define BOILERPLATE(SYMBOL)                                                  \
  template <template <typename...> typename Pointer, typename Top>             \
  auto operator SYMBOL(heterogeneous<Pointer, Top> const& a,                   \
                       heterogeneous<Pointer, Top> const& b) -> decltype(auto) \
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
