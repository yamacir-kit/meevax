/*
   Copyright 2018-2025 Tatsuya Yamasaki.

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

#ifndef INCLUDED_MEEVAX_KERNEL_DYNAMIC_ENVIRONMENT_HPP
#define INCLUDED_MEEVAX_KERNEL_DYNAMIC_ENVIRONMENT_HPP

#include <array>
#include <meevax/kernel/list.hpp>

namespace meevax::inline kernel
{
  struct dynamic_environment
  {
    /*
       Auxiliary register.

       a[0] is used for current-dynamic-extents (dynamic-wind).
       a[1] is used for current-dynamic-bindings (make-parameter and parameterize).
       a[2] is used for current-exception-handlers (with-exception-handler, raise and raise-continuable).
    */
    std::array<let, 3> a;

    /*
       exception_handler is a one-argument procedure for propagating C++
       exceptions thrown in the Meevax kernel to the exception handler of the
       language running on the Meevax kernel.

       exception_handler is set to null by default. In this default state, that
       is, if exception_handler is null, C++ exceptions thrown in the kernel
       are rethrown to the outer environment.

       Although exception_handler can be set to any one-argument procedure by
       procedure `kernel-exception-handler-set!`, it is basically assumed to be
       set to R7RS Scheme's standard procedure `raise`.
    */
    let static inline exception_handler = nullptr;

    auto apply(object const& procedure, auto&&... xs) -> decltype(auto)
    {
      return execute(list(procedure, list(std::forward<decltype(xs)>(xs)...)),
                     nullptr,
                     list(make<instruction>(instruction::secd_call),
                          make<instruction>(instruction::secd_stop)),
                     nullptr);
    }

    auto execute(object const&) -> object;

    auto execute(object, object, object, object) -> object;
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_DYNAMIC_ENVIRONMENT_HPP
