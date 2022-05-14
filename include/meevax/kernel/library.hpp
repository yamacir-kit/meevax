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
#include <unordered_map>

namespace meevax
{
inline namespace kernel
{
  struct library : public environment
  {
    std::vector<object> export_specs;

    template <typename F, REQUIRES(std::is_invocable<F, library &>)>
    explicit library(F&& declare)
      : environment { empty }
    {
      declare(*this);
    }

    explicit library(const_reference declarations)
      : environment { empty }
    {
      for (let const& declaration : declarations)
      {
        declare(declaration);
      }
    }

    static auto boot() -> void;

    auto declare(const_reference declaration) -> void
    {
      if (declaration.is<pair>() and car(declaration).is<symbol>()
                                 and car(declaration).as<symbol>().value == "export")
      {
        for (let const& export_spec : cdr(declaration))
        {
          export_(export_spec);
        }
      }
      else if (declaration.is<pair>() and car(declaration).is<symbol>()
                                      and car(declaration).as<symbol>().value == "begin")
      {
        for (let const& command_or_definition : declaration)
        {
          declare(command_or_definition);
        }
      }
      else
      {
        evaluate(declaration); // Non-standard extension.
      }
    }

    auto export_(const_reference export_spec) -> void
    {
      export_specs.push_back(export_spec);
    }

    template <typename... Ts, REQUIRES(std::is_convertible<Ts, std::string>...)>
    auto export_(Ts&&... xs) -> void
    {
      (export_(read(xs)), ...);
    }

    auto export_to(environment & destination)
    {
      for (let const& export_spec : export_specs)
      {
        if (export_spec.is<pair>() and car(export_spec).is<symbol>()
                                   and car(export_spec).as<symbol>().value == "rename")
        {
        }
        else if (let const& binding = (*this)[export_spec]; binding.is<unbound>())
        {
          std::cout << "; warning: " << export_spec << " is exported but undefined." << std::endl;
        }
        else
        {
          destination.define(export_spec, binding);
        }
      }
    }

    friend auto operator <<(std::ostream & os, library const& library) -> std::ostream &
    {
      return os << library.global();
    }

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

  extern std::map<std::string, library> libraries;

  template <typename... Ts>
  auto define_library(std::string const& name, Ts&&... xs)
  {
    return libraries.emplace(name, std::forward<decltype(xs)>(xs)...);
  }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_LIBRARY_HPP