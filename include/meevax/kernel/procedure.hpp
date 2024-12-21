/*
   Copyright 2018-2024 Tatsuya Yamasaki.

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
#include <meevax/kernel/describable.hpp>
#include <meevax/kernel/ghost.hpp>

namespace meevax::inline kernel
{
  struct primitive : public describable
  {
    using signature = auto (*)(object &) -> object;

    using describable::describable;

    virtual auto operator ()(object & = unit) const -> object = 0;
  };

  auto operator <<(std::ostream &, primitive const&) -> std::ostream &;

  template <typename F>
  struct generic_procedure : public primitive
  {
    std::enable_if_t<std::is_invocable_v<F> or std::is_invocable_v<F, object &>, F> invocable;

    explicit generic_procedure(std::string const& name, F invocable)
      : primitive { name }
      , invocable { invocable }
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

  using thunk = auto (*)() -> object;

  template <typename F>
  struct procedure_traits<F, std::enable_if_t<std::is_convertible_v<F, thunk>>>
  {
    using type = generic_procedure<thunk>;
  };

  using normal_procedure = auto (*)(object const&) -> object;

  template <typename F>
  struct procedure_traits<F, std::enable_if_t<std::is_convertible_v<F, normal_procedure>>>
  {
    using type = generic_procedure<normal_procedure>;
  };

  using linear_update_procedure = auto (*)(object &) -> object;

  template <typename F>
  struct procedure_traits<F, std::enable_if_t<std::is_convertible_v<F, linear_update_procedure>>>
  {
    using type = generic_procedure<linear_update_procedure>;
  };

  using predicate = auto (*)(object const&) -> bool;

  template <typename F>
  struct procedure_traits<F, std::enable_if_t<std::is_convertible_v<F, predicate>>>
  {
    using type = generic_procedure<predicate>;
  };

  using command = auto (*)(object const&) -> void;

  template <typename F>
  struct procedure_traits<F, std::enable_if_t<std::is_convertible_v<F, command>>>
  {
    using type = generic_procedure<command>;
  };

  using mutation = auto (*)(object &) -> void;

  template <typename F>
  struct procedure_traits<F, std::enable_if_t<std::is_convertible_v<F, mutation>>>
  {
    using type = generic_procedure<mutation>;
  };

  template <typename, typename... Ts>
  using procedure = procedure_traits<Ts...>;
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP
