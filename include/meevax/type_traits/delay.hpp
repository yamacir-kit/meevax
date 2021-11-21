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

#ifndef INCLUDED_MEEVAX_TYPE_TRAITS_DELAY_HPP
#define INCLUDED_MEEVAX_TYPE_TRAITS_DELAY_HPP

#include <meevax/kernel/overview.hpp> // for raise
#include <meevax/utility/demangle.hpp>

namespace meevax
{
inline namespace type_traits
{
  template <typename F>
  struct delay
  {
    static inline F invoke {};

    template <typename T, typename = void>
    struct viable_1
      : public std::false_type
    {};

    template <typename T>
    struct viable_1<T, std::void_t<decltype(std::invoke(std::declval<F>(), std::declval<T>()))>>
      : public std::true_type
    {};

    template <typename T, typename U, typename = void>
    struct viable_2
      : public std::false_type
    {};

    template <typename T, typename U>
    struct viable_2<T, U, std::void_t<decltype(std::invoke(std::declval<F>(), std::declval<T>(), std::declval<U>()))>>
      : public std::true_type
    {};

    template <typename T, typename = void>
    struct select_1
    {
      template <typename R>
      static auto apply(T&&) -> R
      {
        if constexpr (std::is_same<R, bool>::value)
        {
          return false;
        }
        else
        {
          std::stringstream ss {};
          ss << "no viable operation " << demangle(typeid(F)) << " with " << demangle(typeid(T));
          raise(ss.str());
        }
      }
    };

    template <typename T>
    struct select_1<T, typename std::enable_if<viable_1<T>::value>::type>
    {
      template <typename R>
      static constexpr auto apply(T&& x) -> R
      {
        return invoke(std::forward<decltype(x)>(x));
      }
    };

    template <typename T, typename U, typename = void>
    struct select_2
    {
      template <typename R>
      static auto apply(T&&, U&&) -> R
      {
        if constexpr (std::is_same<R, bool>::value)
        {
          return false;
        }
        else
        {
          std::stringstream ss {};
          ss << "no viable operation " << demangle(typeid(F)) << " with " << demangle(typeid(T)) << " and " << demangle(typeid(U));
          raise(ss.str());
        }
      }
    };

    template <typename T, typename U>
    struct select_2<T, U, typename std::enable_if<viable_2<T, U>::value>::type>
    {
      template <typename R>
      static constexpr auto apply(T&& x, U&& y) -> R
      {
        return invoke(std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
      }
    };

    template <typename R, typename T>
    static constexpr auto yield(T&& x) -> decltype(auto)
    {
      return select_1<T>().template apply<R>(std::forward<decltype(x)>(x));
    }

    template <typename R, typename T, typename U>
    static constexpr auto yield(T&& x, U&& y) -> decltype(auto)
    {
      return select_2<T, U>().template apply<R>(std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
    }
  };
} // namespace type_traits
} // namespace meevax

#endif // INCLUDED_MEEVAX_TYPE_TRAITS_DELAY_HPP
