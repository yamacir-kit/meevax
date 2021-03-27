#ifndef INCLUDED_MEEVAX_KERNEL_OBJECT_HPP
#define INCLUDED_MEEVAX_KERNEL_OBJECT_HPP

#include <meevax/kernel/pointer.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename T>
  struct alignas(sizeof(word)) top
  {
    virtual auto type() const noexcept -> std::type_info const&
    {
      return typeid(T);
    }

    virtual bool eqv(pointer<T> const& rhs) const
    {
      if constexpr (is_equality_comparable<T>::value)
      {
        auto const p = std::dynamic_pointer_cast<T const>(rhs);
        return p and *p == static_cast<T const&>(*this);
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
    virtual auto operator SYMBOL(pointer<T> const& x) const -> RESULT          \
    {                                                                          \
      return delay<FUNCTOR>().yield<RESULT>(static_cast<T const&>(*this), x);  \
    } static_assert(true)

    BOILERPLATE(+, pointer<T>, std::plus<void>);
    BOILERPLATE(-, pointer<T>, std::minus<void>);
    BOILERPLATE(*, pointer<T>, std::multiplies<void>);
    BOILERPLATE(/, pointer<T>, std::divides<void>);
    BOILERPLATE(%, pointer<T>, std::modulus<void>);

    BOILERPLATE(==, bool, std::equal_to<void>);
    BOILERPLATE(!=, bool, std::not_equal_to<void>);
    BOILERPLATE(<,  bool, std::less<void>);
    BOILERPLATE(<=, bool, std::less_equal<void>);
    BOILERPLATE(>,  bool, std::greater<void>);
    BOILERPLATE(>=, bool, std::greater_equal<void>);

    #undef BOILERPLATE
  };

  // TODO Rename to 'cons'?
  using resource = std::allocator<object>;

  template <typename T, typename... Ts>
  inline constexpr decltype(auto) make(Ts&&... xs)
  {
    return object::allocate<T>(std::forward<decltype(xs)>(xs)...);
  }

  template <typename T>
  inline constexpr auto make(T&& x)
  {
    return object::allocate<typename std::decay<T>::type>(std::forward<decltype(x)>(x));
  }

  // #if __cpp_lib_memory_resource
  // template <typename T,
  //           typename MemoryResource, // XXX (GCC-9 <=)
  //           typename... Ts>
  // inline constexpr decltype(auto) allocate(MemoryResource&& resource, Ts&&... xs)
  // {
  //   return
  //     object::allocate_binding<T>(
  //       std::forward<decltype(resource)>(resource),
  //       std::forward<decltype(xs)>(xs)...);
  // }
  // #endif // __cpp_lib_memory_resource

  let extern const unit;

  template <typename T> using is_object    = std::is_base_of<                       object       , typename std::decay<T>::type>;
  template <typename T> using is_reference = std::is_base_of<std::reference_wrapper<object const>, typename std::decay<T>::type>;

  auto unwrap = [](auto&& x) -> decltype(auto)
  {
    if constexpr (is_object<decltype(x)>::value)
    {
      return x.binding();
    }
    else if constexpr (is_reference<decltype(x)>::value)
    {
      return x.get().binding();
    }
    else
    {
      return std::forward<decltype(x)>(x);
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_OBJECT_HPP
