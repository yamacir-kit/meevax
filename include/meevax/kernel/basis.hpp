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

#include <string_view>

namespace meevax
{
inline namespace kernel
{
  #if __cpp_lib_string_view
  using string_view = std::string_view;
  #else
  using string_view = std::experimental::string_view;
  #endif

  extern string_view const overture;
  extern string_view const r7rs;
  extern string_view const srfi_1;
  extern string_view const srfi_8;
  extern string_view const srfi_23;
  extern string_view const srfi_34;
  extern string_view const srfi_39;
  extern string_view const srfi_45;
  extern string_view const srfi_78;
  extern string_view const srfi_149;
  extern string_view const srfi_211;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_LIBRARY_HPP
