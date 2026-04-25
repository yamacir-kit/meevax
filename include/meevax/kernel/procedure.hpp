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

#ifndef INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP
#define INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP

#include <meevax/kernel/symbol.hpp>

namespace meevax::inline kernel
{
  struct procedure
  {
    using signature = auto (*)(object const&) -> object;

    symbol name;

    signature call;

    explicit procedure(std::string const& name, signature call)
      : name { name }
      , call { call }
    {}

    explicit procedure(std::string const&, std::string const&);
  };

  auto operator <<(std::ostream &, procedure const&) -> std::ostream &;
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP
