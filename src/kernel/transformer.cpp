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

#include <meevax/kernel/transformer.hpp>

namespace meevax
{
inline namespace kernel
{
  auto transformer::closure() const -> object const&
  {
    return first;
  }

  auto transformer::syntactic_environment() const -> object const&
  {
    return second;
  }

  auto operator <<(std::ostream & os, transformer const& datum) -> std::ostream &
  {
    return os << magenta("#,(") << green("transformer ") << faint("#;", &datum) << magenta(")");
  }
} // namespace kernel
} // namespace meevax
