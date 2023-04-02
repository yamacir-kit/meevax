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

#include <meevax/kernel/list.hpp>
#include <meevax/kernel/ghost.hpp>

namespace meevax
{
inline namespace kernel
{
  #define PROCEDURE(...) meevax::object __VA_ARGS__(meevax::object const& xs)

  struct procedure
  {
    std::string const name;

    std::function<PROCEDURE()> const call;

    template <typename F, std::enable_if_t<
                            std::is_same_v<std::invoke_result_t<F, let const&>, object>,
                            std::nullptr_t
                          > = nullptr>
    explicit procedure(std::string const& name, F&& f)
      : name { name }
      , call { std::forward<decltype(f)>(f) }
    {}

    template <typename F, std::enable_if_t<
                            std::is_same_v<std::invoke_result_t<F>, object>,
                            std::nullptr_t
                          > = nullptr>
    explicit procedure(std::string const& name, F&& f)
      : name { name }
      , call {
          [f](auto&&...)
          {
            return f();
          }
        }
    {}

    template <typename F, std::enable_if_t<
                            std::is_same_v<std::invoke_result_t<F, let const&>, bool>,
                            std::nullptr_t
                          > = nullptr>
    explicit procedure(std::string const& name, F&& f)
      : name { name }
      , call {
          [f](auto&&... xs)
          {
            return f(std::forward<decltype(xs)>(xs)...) ? t : meevax::f;
          }
        }
    {}

    template <typename F, std::enable_if_t<
                            std::is_same_v<std::invoke_result_t<F>, bool>,
                            std::nullptr_t
                          > = nullptr>
    explicit procedure(std::string const& name, F&& f)
      : name { name }
      , call {
          [f](auto&&...)
          {
            return f() ? t : meevax::f;
          }
        }
    {}

    template <typename F, std::enable_if_t<
                            std::is_same_v<std::invoke_result_t<F, let const&>, void>,
                            std::nullptr_t
                          > = nullptr>
    explicit procedure(std::string const& name, F&& f)
      : name { name }
      , call {
          [f](auto&&... xs)
          {
            f(std::forward<decltype(xs)>(xs)...);
            return unspecified;
          }
        }
    {}

    template <typename F, std::enable_if_t<
                            std::is_same_v<std::invoke_result_t<F>, void>,
                            std::nullptr_t
                          > = nullptr>
    explicit procedure(std::string const& name, F&& f)
      : name { name }
      , call {
          [f](auto&&...)
          {
            f();
            return unspecified;
          }
        }
    {}

    explicit procedure(std::string const&, std::string const&);

    static auto dlopen(std::string const&) -> void *;

    static auto dlsym(std::string const&, void * const) -> PROCEDURE((*));
  };

  auto operator <<(std::ostream &, procedure const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP
