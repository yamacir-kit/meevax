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

#ifndef INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP
#define INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP

#include <meevax/kernel/list.hpp>
#include <meevax/kernel/symbol.hpp>
#include <meevax/utility/description.hpp>

namespace meevax
{
inline namespace kernel
{
  #define PROCEDURE(...) meevax::value_type __VA_ARGS__(meevax::const_reference xs)

  struct procedure : public description
  {
    using function_pointer_type = PROCEDURE((*));

    using function_type = std::function<PROCEDURE()>;

    function_type call;

    explicit procedure(symbol::value_type const&, function_type const&);

    explicit procedure(symbol::value_type const&, symbol::value_type const&);

    static auto dlopen(symbol::value_type const&) -> void *;

    static auto dlsym(symbol::value_type const&, void * const) -> function_pointer_type;
  };

  auto operator <<(std::ostream &, procedure const&) -> std::ostream &;

  struct predicate : public procedure
  {
    template <typename Callable>
    explicit predicate(symbol::value_type const& name, Callable && call)
      : procedure { name, [call](auto&&... xs) { return call(std::forward<decltype(xs)>(xs)...) ? t : f; } }
    {}
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP
