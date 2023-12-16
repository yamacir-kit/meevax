/*
   Copyright 2018-2023 Tatsuya Yamasaki.

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

#ifndef INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP
#define INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP

#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/ghost.hpp>
#include <meevax/kernel/list.hpp>

namespace meevax
{
inline namespace kernel
{
  struct primitive_procedure
  {
    using signature = auto (*)(object &) -> object;

    std::string const name;

    explicit primitive_procedure(std::string const& name)
      : name { name }
    {}

    virtual auto operator ()(object & = unit) const -> object = 0;
  };

  auto operator <<(std::ostream &, primitive_procedure const&) -> std::ostream &;

  template <typename F>
  struct generic_procedure : public primitive_procedure
  {
    std::enable_if_t<std::is_invocable_v<F> or std::is_invocable_v<F, object &>, F> invocable;

    template <typename T, typename U>
    explicit generic_procedure(T && x, U && y)
      : primitive_procedure { std::forward<decltype(x)>(x) }
      , invocable           { std::forward<decltype(y)>(y) }
    {}

    auto operator ()(object & xs) const -> object override
    {
      if constexpr (std::is_invocable_v<F>)
      {
        if constexpr (std::is_same_v<std::invoke_result_t<F>, void>)
        {
          std::invoke(invocable);
          return unspecified;
        }
        else if constexpr (std::is_same_v<std::invoke_result_t<F>, bool>)
        {
          return std::invoke(invocable) ? t : f;
        }
        else
        {
          return std::invoke(invocable);
        }
      }
      else
      {
        if constexpr (std::is_same_v<std::invoke_result_t<F, object &>, void>)
        {
          std::invoke(invocable, xs);
          return unspecified;
        }
        else if constexpr (std::is_same_v<std::invoke_result_t<F, object &>, bool>)
        {
          return std::invoke(invocable, xs) ? t : f;
        }
        else
        {
          return std::invoke(invocable, xs);
        }
      }
    }
  };

  /*
     There is no need to define specializations for this primary template to
     work. However, since the template parameter `F` is usually given a
     closure, there will be a unique template instantiation for each lambda
     expression. The problem in this case is that the binary size becomes huge.
     To deal with this, we take advantage of the fact that closures that do not
     capture anything can be converted to function pointers, and decay closures
     into function pointer types to reduce the number of template
     instantiations.
  */
  template <typename F, typename = void>
  struct procedure_traits
  {
    using type = generic_procedure<F>;
  };

  /*
     Thunk
  */
  template <typename F>
  struct procedure_traits<F, std::enable_if_t<std::is_convertible_v<F, auto (*)() -> object>>>
  {
    using type = generic_procedure<auto (*)() -> object>;
  };

  /*
     Procedure
  */
  template <typename F>
  struct procedure_traits<F, std::enable_if_t<std::is_convertible_v<F, auto (*)(object const&) -> object>>>
  {
    using type = generic_procedure<auto (*)(object const&) -> object>;
  };

  /*
     Linear update procedure
  */
  template <typename F>
  struct procedure_traits<F, std::enable_if_t<std::is_convertible_v<F, auto (*)(object &) -> object>>>
  {
    using type = generic_procedure<auto (*)(object &) -> object>;
  };

  /*
     Predicate
  */
  template <typename F>
  struct procedure_traits<F, std::enable_if_t<std::is_convertible_v<F, auto (*)(object const&) -> bool>>>
  {
    using type = generic_procedure<auto (*)(object const&) -> bool>;
  };

  /*
     Command
  */
  template <typename F>
  struct procedure_traits<F, std::enable_if_t<std::is_convertible_v<F, auto (*)(object const&) -> void>>>
  {
    using type = generic_procedure<auto (*)(object const&) -> void>;
  };

  /*
     Mutation
  */
  template <typename F>
  struct procedure_traits<F, std::enable_if_t<std::is_convertible_v<F, auto (*)(object &) -> void>>>
  {
    using type = generic_procedure<auto (*)(object &) -> void>;
  };

  template <typename T, typename... Ts>
  using procedure = std::enable_if_t<std::is_convertible_v<T, std::string>, procedure_traits<Ts...>>;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP
