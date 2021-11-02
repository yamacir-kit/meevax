/*
   Copyright 2018-2021 Tatsuya Yamasaki.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

#ifndef INCLUDED_MEEVAX_KERNEL_POINTER_HPP
#define INCLUDED_MEEVAX_KERNEL_POINTER_HPP

#include <meevax/functional/addition.hpp>
#include <meevax/functional/compose.hpp>
#include <meevax/functional/division.hpp>
#include <meevax/functional/modulo.hpp>
#include <meevax/functional/multiplication.hpp>
#include <meevax/functional/numerical.hpp>
#include <meevax/functional/subtraction.hpp>
#include <meevax/posix/vt10x.hpp>
#include <meevax/type_traits/delay.hpp>
#include <meevax/type_traits/is_equality_comparable.hpp>
#include <meevax/utility/module.hpp>

namespace meevax
{
inline namespace kernel
{
  template <template <typename...> typename Pointer, typename Top>
  class heterogeneous : public Pointer<Top>
  {
    template <typename Bound>
    struct binder : public virtual Top
                  , public Bound
    {
      template <typename... Ts>
      explicit constexpr binder(Ts&&... xs)
        : std::conditional<std::is_base_of<Top, Bound>::value, Top, Bound>::type { std::forward<decltype(xs)>(xs)... }
      {}

      ~binder() override = default;

      auto compare(heterogeneous const& x) const -> bool override
      {
        if constexpr (is_equality_comparable<Bound>::value)
        {
          if (auto const* address = dynamic_cast<Bound const*>(x.get()); address)
          {
            return *address == static_cast<Bound const&>(*this);
          }
          else
          {
            return std::is_same<Bound, null>::value;
          }
        }
        else
        {
          return false;
        }
      }

      auto is_nan() const -> bool override
      {
        return delay<is_nan_t>().yield<bool>(static_cast<Bound const&>(*this));
      }

      auto type() const noexcept -> std::type_info const& override
      {
        return typeid(Bound);
      }

      auto write(std::ostream & os) const -> std::ostream & override
      {
        return delay<write_t>().yield<decltype(os)>(os, static_cast<Bound const&>(*this));
      }

      #define BOILERPLATE(SYMBOL, RESULT, FUNCTION)                            \
      auto operator SYMBOL(heterogeneous const& x) const -> RESULT override    \
      {                                                                        \
        return delay<FUNCTION>().yield<RESULT>(static_cast<Bound const&>(*this), x); \
      } static_assert(true)

      BOILERPLATE(+, heterogeneous, addition);
      BOILERPLATE(-, heterogeneous, subtraction);
      BOILERPLATE(*, heterogeneous, multiplication);
      BOILERPLATE(/, heterogeneous, division);
      BOILERPLATE(%, heterogeneous, modulo);

      BOILERPLATE(==, bool, std::equal_to     <void>);
      BOILERPLATE(!=, bool, std::not_equal_to <void>);
      BOILERPLATE(<,  bool, std::less         <void>);
      BOILERPLATE(<=, bool, std::less_equal   <void>);
      BOILERPLATE(>,  bool, std::greater      <void>);
      BOILERPLATE(>=, bool, std::greater_equal<void>);

      #undef BOILERPLATE

      #define DEFINE(NAME)                                                     \
      auto NAME() const -> heterogeneous override                              \
      {                                                                        \
        return delay<NAME##_t>().yield<heterogeneous>(static_cast<Bound const&>(*this)); \
      }                                                                        \
      static_assert(true)

      DEFINE(exact);
      DEFINE(inexact);

      DEFINE(sin); DEFINE(asin); DEFINE(sinh); DEFINE(asinh); DEFINE(exp);
      DEFINE(cos); DEFINE(acos); DEFINE(cosh); DEFINE(acosh); DEFINE(log);
      DEFINE(tan); DEFINE(atan); DEFINE(tanh); DEFINE(atanh); DEFINE(sqrt);

      #undef DEFINE

      #define DEFINE(NAME)                                                     \
      auto NAME(heterogeneous const& x) const -> heterogeneous override        \
      {                                                                        \
        return delay<NAME##_t>().yield<heterogeneous>(static_cast<Bound const&>(*this), x); \
      }                                                                        \
      static_assert(true)

      DEFINE(atan2);
      DEFINE(pow);

      #undef DEFINE

      #define PREDICATE(NAME)                                                  \
      auto NAME() const -> bool override                                       \
      {                                                                        \
        return delay<NAME##_t>().yield<bool>(static_cast<Bound const&>(*this)); \
      }                                                                        \
      static_assert(true)

      PREDICATE(is_integer);

      #undef PREDICATE
    };

  public:
    using Pointer<Top>::Pointer;
    using Pointer<Top>::get;
    using Pointer<Top>::load;

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

    template <typename U>
    inline auto as() const -> U &
    {
      if (auto data = dynamic_cast<pointer<U>>(get()); data)
      {
        return *data;
      }
      else
      {
        std::stringstream ss {};
        ss << "no viable conversion from " << demangle(type()) << " to " << demangle(typeid(U));
        raise(ss.str());
      }
    }

    template <typename U>
    inline auto as_const() const -> U const&
    {
      return as<const U>();
    }

    inline auto compare(heterogeneous const& rhs) const -> bool
    {
      return type() == rhs.type() and load().compare(rhs);
    }

    template <typename U>
    inline auto is() const
    {
      if constexpr (std::is_null_pointer<typename std::decay<U>::type>::value)
      {
        return not static_cast<bool>(*this);
      }
      else
      {
        return type() == typeid(typename std::decay<U>::type);
      }
    }

    template <typename U>
    inline auto is_also() const
    {
      return dynamic_cast<pointer<U>>(get()) != nullptr;
    }

    inline auto is_nan() const
    {
      return not is<null>() and load().is_nan();
    }

    inline auto type() const -> std::type_info const&
    {
      return *this ? load().type() : typeid(null);
    }

    #define DEFINE(NAME)                                                       \
    template <typename... Ts>                                                  \
    inline auto NAME(Ts&&... xs) const                                         \
    {                                                                          \
      if (not is<null>())                                                      \
      {                                                                        \
        return load().NAME(std::forward<decltype(xs)>(xs)...);                 \
      }                                                                        \
      else                                                                     \
      {                                                                        \
        std::stringstream ss {};                                               \
        ss << "no viable operation " #NAME " for " << *this;                   \
        raise(ss.str());                                                       \
      }                                                                        \
    }                                                                          \
    static_assert(true)

    DEFINE(is_integer);

    DEFINE(exact);
    DEFINE(inexact);

    DEFINE(sin); DEFINE(asin); DEFINE(sinh); DEFINE(asinh); DEFINE(exp);
    DEFINE(cos); DEFINE(acos); DEFINE(cosh); DEFINE(acosh); DEFINE(log);
    DEFINE(tan); DEFINE(atan); DEFINE(tanh); DEFINE(atanh); DEFINE(pow);
                 DEFINE(atan2);                             DEFINE(sqrt);

    #undef DEFINE
  };

  template <template <typename...> typename Pointer, typename Top>
  auto operator <<(std::ostream & os, heterogeneous<Pointer, Top> const& datum) -> std::ostream &
  {
    return (datum.template is<null>() ? os << magenta << "()" : datum.load().write(os)) << reset;
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
      std::stringstream ss {};                                                 \
      ss << "no viable operation " #SYMBOL " with " << a << " and " << b;      \
      raise(ss.str());                                                         \
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
