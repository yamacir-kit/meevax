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
  #define PROCEDURE(...) meevax::object __VA_ARGS__(meevax::object const& xs)

  struct procedure
  {
    std::string const name;

    std::function<PROCEDURE()> const function;

    template <typename F, std::enable_if_t<
                            std::is_same_v<std::invoke_result_t<F, let const&>, object>,
                            std::nullptr_t
                          > = nullptr>
    explicit procedure(std::string const& name, F&& f)
      : name { name }
      , function { std::forward<decltype(f)>(f) }
    {}

    explicit procedure(std::string const&, std::string const&);

    static auto dlopen(std::string const&) -> void *;

    static auto dlsym(std::string const&, void * const) -> PROCEDURE((*));

    virtual auto operator ()(object & xs = unit) const -> object
    {
      return function(xs);
    }
  };

  auto operator <<(std::ostream &, procedure const&) -> std::ostream &;

  struct predicate : public procedure
  {
    bool (*test)(object const&);

    template <typename Tester>
    explicit predicate(std::string const& name, Tester test)
      : procedure { name, [](let const&) { return unspecified; } }
      , test { test }
    {}

    auto operator ()(object & xs) const -> object override
    {
      return test(xs) ? t : f;
    }
  };

  struct modifier : public procedure // mutation-procedure
  {
    void (*modify)(object &);

    template <typename Modifier>
    explicit modifier(std::string const& name, Modifier modify)
      : procedure { name, [](let const&) { return unspecified; } }
      , modify { modify }
    {}

    auto operator ()(object & xs) const -> object override
    {
      modify(xs);
      return unspecified;
    }
  };

  struct command : public procedure
  {
    void (*function)(object const&);

    template <typename Name, typename Function>
    explicit command(Name&& name, Function function)
      : procedure { std::forward<decltype(name)>(name), [](let const&) { return unspecified; } }
      , function { function }
    {}

    auto operator ()(object & xs) const -> object override
    {
      function(xs);
      return unspecified;
    }
  };

  struct thunk : public procedure
  {
    object (*function)();

    template <typename Name, typename Function>
    explicit thunk(Name&& name, Function function)
      : procedure { std::forward<decltype(name)>(name), [](let const&) { return unspecified; } }
      , function { function }
    {}

    auto operator ()(object &) const -> object override
    {
      return function();
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP
