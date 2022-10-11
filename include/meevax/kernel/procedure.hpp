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

namespace meevax
{
inline namespace kernel
{
  #define PROCEDURE(...) meevax::value_type __VA_ARGS__(meevax::const_reference xs)

  struct procedure
  {
    using function_pointer_type = PROCEDURE((*));

    using function_type = std::function<PROCEDURE()>;

    std::string const name;

    function_type call;

    explicit procedure(std::string const&, function_type const&);

    explicit procedure(std::string const&, std::string const&);

    static auto dlopen(std::string const&) -> void *;

    static auto dlsym(std::string const&, void * const) -> function_pointer_type;
  };

  auto operator <<(std::ostream &, procedure const&) -> std::ostream &;

  struct predicate : public procedure
  {
    template <typename Callable>
    explicit predicate(std::string const& name, Callable && call)
      : procedure { name, [call](auto&&... xs) { return call(std::forward<decltype(xs)>(xs)...) ? t : f; } }
    {}
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP
