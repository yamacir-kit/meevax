/*
   Copyright 2018-2022 Tatsuya Yamasaki.

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

#ifndef INCLUDED_MEEVAX_KERNEL_LIBRARY_HPP
#define INCLUDED_MEEVAX_KERNEL_LIBRARY_HPP

#include <meevax/kernel/environment.hpp>

namespace meevax
{
inline namespace kernel
{
  struct library : public environment
  {
    let const declarations = unit;

    struct export_spec
    {
      let const form;

      explicit export_spec(const_reference form)
        : form { form }
      {}

      auto identify(environment & library_environment) const
      {
        if (form.is<pair>())
        {
          assert(form[0].is<symbol>());
          assert(form[0].as<symbol>().value == "rename");
          return make<absolute>(form[2], library_environment[form[1]]);
        }
        else
        {
          return library_environment.identify(form, unit);
        }
      }
    };

    struct export_specs : public std::vector<export_spec>
    {
      let import_set = unit;

      auto make_import_set(environment & e) -> decltype(auto)
      {
        return import_set.is<null>() ? import_set = std::accumulate(std::begin(*this), std::end(*this), unit, [&](auto&& xs, auto&& export_spec)
                                       {
                                         return cons(export_spec.identify(e), xs);
                                       })
                                     : import_set;
      }
    };

    std::tuple<export_specs> declaration;

    template <typename F, REQUIRES(std::is_invocable<F, library &>)>
    explicit library(F&& f)
    {
      std::invoke(std::forward<decltype(f)>(f), *this);
    }

    explicit library(const_reference);

    static auto boot() -> void;

    auto build() -> void;

    template <typename T, typename... Ts>
    auto declare(Ts&&... xs) -> void
    {
      std::get<export_specs>(declaration).emplace_back(std::forward<decltype(xs)>(xs)...);
    }

    template <typename T, typename... Ts>
    auto define(std::string const& name, Ts&&... xs) -> void
    {
      environment::define<T>(name, std::forward<decltype(xs)>(xs)...);
      declare<export_spec>(read(name));
    }

    auto evaluate(const_reference) -> void;

    auto resolve() -> const_reference;
  };

  auto operator <<(std::ostream &, library const&) -> std::ostream &;

  extern std::unordered_map<std::string, library> libraries;

  template <typename... Ts>
  auto define_library(std::string const& name, Ts&&... xs)
  {
    return libraries.emplace(name, std::forward<decltype(xs)>(xs)...);
  }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_LIBRARY_HPP
