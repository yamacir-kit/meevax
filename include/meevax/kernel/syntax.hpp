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

#ifndef INCLUDED_MEEVAX_KERNEL_SYNTAX_HPP
#define INCLUDED_MEEVAX_KERNEL_SYNTAX_HPP

#include <meevax/kernel/syntactic_environment.hpp>

namespace meevax::inline kernel
{
  struct syntax : public describable
  {
    auto (*expand)(syntactic_environment const& expander,
                   object const& form,
                   object const& bound_variables,
                   syntactic_closure::renamer & rename) -> object;

    auto (*generate)(syntactic_environment & generator,
                     object const& form,
                     object const& bound_variables,
                     object const& continuation,
                     bool tail) -> object;

    template <typename Expander, typename Generator>
    explicit syntax(std::string const& name, Expander const& expand, Generator const& generate)
      : describable { name }
      , expand { expand }
      , generate { generate }
    {}
  };

  auto operator <<(std::ostream &, syntax const&) -> std::ostream &;
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_SYNTAX_HPP
