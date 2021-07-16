#ifndef INCLUDED_MEEVAX_KERNEL_OBJECT_HPP
#define INCLUDED_MEEVAX_KERNEL_OBJECT_HPP

#include <cstddef>
#include <meevax/kernel/heterogeneous.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename T>
  struct top
  {
    using let = heterogeneous<cell, T>;

    virtual auto type() const noexcept -> std::type_info const&
    {
      return typeid(T);
    }

    virtual bool eqv(let const& x) const
    {
      if constexpr (is_equality_comparable<T>::value)
      {
        if (auto const* address = dynamic_cast<T const*>(x.get()); address)
        {
          return *address == static_cast<T const&>(*this);
        }
        else
        {
          return std::is_same<T, std::nullptr_t>::value;
        }
      }
      else
      {
        return false;
      }
    }

    virtual auto write_to(output_port & port) const -> output_port &
    {
      return delay<write>().yield<output_port &>(port, static_cast<T const&>(*this));
    }

    #define BOILERPLATE(SYMBOL, RESULT, FUNCTOR)                               \
    virtual auto operator SYMBOL(let const& x) const -> RESULT                 \
    {                                                                          \
      return delay<FUNCTOR>().yield<RESULT>(static_cast<T const&>(*this), x);  \
    } static_assert(true)

    BOILERPLATE(+, let, std::plus      <void>);
    BOILERPLATE(-, let, std::minus     <void>);
    BOILERPLATE(*, let, std::multiplies<void>);
    BOILERPLATE(/, let, std::divides   <void>);
    BOILERPLATE(%, let, std::modulus   <void>);

    BOILERPLATE(!=, bool, std::not_equal_to <void>);
    BOILERPLATE(<,  bool, std::less         <void>);
    BOILERPLATE(<=, bool, std::less_equal   <void>);
    BOILERPLATE(==, bool, std::equal_to     <void>);
    BOILERPLATE(>,  bool, std::greater      <void>);
    BOILERPLATE(>=, bool, std::greater_equal<void>);

    #undef BOILERPLATE
  };

  template <typename T, typename... Ts>
  inline constexpr auto make(Ts&&... xs)
  {
    return let::allocate<T>(std::forward<decltype(xs)>(xs)...);
  }

  template <typename T>
  inline constexpr auto make(T&& x)
  {
    return let::allocate<typename std::decay<T>::type>(std::forward<decltype(x)>(x));
  }

  template <typename T> using is_object    = std::is_base_of<                       let       , typename std::decay<T>::type>;
  template <typename T> using is_reference = std::is_base_of<std::reference_wrapper<let const>, typename std::decay<T>::type>;

  auto unwrap = [](auto&& x) -> decltype(auto)
  {
    if constexpr (is_object<decltype(x)>::value)
    {
      return x.load();
    }
    else if constexpr (is_reference<decltype(x)>::value)
    {
      return x.get().load();
    }
    else
    {
      return std::forward<decltype(x)>(x);
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_OBJECT_HPP
