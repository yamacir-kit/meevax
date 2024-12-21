/*
   Copyright 2018-2024 Tatsuya Yamasaki.

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

#ifndef INCLUDED_MEEVAX_UTILITY_DEMANGLE_HPP
#define INCLUDED_MEEVAX_UTILITY_DEMANGLE_HPP

#include <string>
#include <typeinfo>

namespace meevax::inline utility
{
  auto demangle(char const* const name) -> std::string;

  auto demangle(std::type_info const&) -> std::string;
} // namespace meevax::utility

#endif // INCLUDED_MEEVAX_UTILITY_DEMANGLE_HPP
