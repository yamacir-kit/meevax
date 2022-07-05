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

#include <functional>
#include <meevax/kernel/environment.hpp>
#include <unordered_map>

namespace meevax
{
inline namespace kernel
{
  struct library : public environment
  {
    let const declarations = unit;

    let export_specs = unit;

    let identifiers = unit;

    template <typename Build, REQUIRES(std::is_invocable<Build, library &>)>
    explicit library(Build&& build)
    {
      build(*this);
    }

    explicit library(const_reference);

    static auto boot() -> void;

    auto build() -> void;

    auto evaluate(const_reference) -> void;

    auto export_(const_reference) -> void;

    auto export_(external_representation const&) -> void;

    auto exported_identifiers() -> const_reference;

    #define DEFINE_BASIS_LIBRARY(NAME)                                         \
    struct NAME##_library_t                                                    \
    {                                                                          \
      explicit NAME##_library_t() = default;                                   \
    }                                                                          \
    static constexpr NAME##_library {};                                        \
                                                                               \
    explicit library(NAME##_library_t)

    DEFINE_BASIS_LIBRARY(character);
    DEFINE_BASIS_LIBRARY(context);
    DEFINE_BASIS_LIBRARY(control);
    DEFINE_BASIS_LIBRARY(environment);
    DEFINE_BASIS_LIBRARY(equivalence);
    DEFINE_BASIS_LIBRARY(evaluate);
    DEFINE_BASIS_LIBRARY(exception);
    DEFINE_BASIS_LIBRARY(experimental);
    DEFINE_BASIS_LIBRARY(inexact);
    DEFINE_BASIS_LIBRARY(list);
    DEFINE_BASIS_LIBRARY(macro);
    DEFINE_BASIS_LIBRARY(number);
    DEFINE_BASIS_LIBRARY(pair);
    DEFINE_BASIS_LIBRARY(port);
    DEFINE_BASIS_LIBRARY(read);
    DEFINE_BASIS_LIBRARY(string);
    DEFINE_BASIS_LIBRARY(symbol);
    DEFINE_BASIS_LIBRARY(syntax);
    DEFINE_BASIS_LIBRARY(vector);
    DEFINE_BASIS_LIBRARY(write);

    #undef DEFINE_BASIS_LIBRARY
  };

  auto operator <<(std::ostream &, library const&) -> std::ostream &;

  extern std::unordered_map<external_representation, library> libraries;

  template <typename... Ts>
  auto define_library(external_representation const& name, Ts&&... xs)
  {
    return libraries.emplace(name, std::forward<decltype(xs)>(xs)...);
  }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_LIBRARY_HPP
