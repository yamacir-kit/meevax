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
  struct callable
  {
    std::string const name;

    explicit callable(std::string const& name)
      : name { name }
    {}

    virtual auto operator ()(object & = unit) const -> object = 0;
  };

  auto operator <<(std::ostream &, callable const&) -> std::ostream &;

  template <typename, typename Callee>
  struct procedure : public callable
  {
    Callee call;

    explicit procedure(std::string const& name, Callee call)
      : callable { name }
      , call { call }
    {}

    auto operator ()(object & xs) const -> object override
    {
      if constexpr (std::is_invocable_v<Callee>)
      {
        if constexpr (std::is_same_v<std::invoke_result_t<Callee>, void>)
        {
          call();
          return unspecified;
        }
        else if constexpr (std::is_same_v<std::invoke_result_t<Callee>, bool>)
        {
          return call() ? t : f;
        }
        else
        {
          return call();
        }
      }
      else
      {
        if constexpr (std::is_same_v<std::invoke_result_t<Callee, object &>, void>)
        {
          call(xs);
          return unspecified;
        }
        else if constexpr (std::is_same_v<std::invoke_result_t<Callee, object &>, bool>)
        {
          return call(xs) ? t : f;
        }
        else
        {
          return call(xs);
        }
      }
    }
  };

  using procedure_pointer = auto (*)(object const&) -> object;

  auto dlopen(std::string const&) -> void *;

  auto dlsym(std::string const&, void * const) -> procedure_pointer;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP
