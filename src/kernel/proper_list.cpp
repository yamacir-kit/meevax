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

#include <meevax/kernel/proper_list.hpp>

namespace meevax::inline kernel
{
  static_assert(std::ranges::range<proper_list_view<pair>>);
  static_assert(std::ranges::range<proper_list_view<pair const>>);

  static_assert(std::ranges::input_range<proper_list_view<pair>>);
  static_assert(std::ranges::input_range<proper_list_view<pair const>>);
} // namespace meevax::kernel
