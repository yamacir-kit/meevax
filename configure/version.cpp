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

#include <meevax/kernel/version.hpp>

namespace meevax
{
inline namespace kernel
{
  auto help() noexcept -> std::string_view
  {
    return R"(${${PROJECT_NAME}_HELP_TEXT})";
  }

  auto features() -> object const&
  {
    let static const features = list(
      make_symbol("r5rs"),
      make_symbol("exact-closed"),
      make_symbol("exact-complex"),
      make_symbol("ieee-float"),
      make_symbol("ratios"),
      make_symbol("posix"),
      make_symbol("${CMAKE_SYSTEM_NAME}"),
      make_symbol("${CMAKE_SYSTEM_PROCESSOR}"),
      // TODO C memory model flags.
      make_symbol("${${PROJECT_NAME}_BYTE_ORDER}"),
      make_symbol("${PROJECT_NAME}"), // The name of this implementation.
      make_symbol("${PROJECT_NAME}-${PROJECT_VERSION}") // The name and version of this implementation.
      );

    return features;
  }

  auto version() -> object const&
  {
    let static const version = make_symbol("${PROJECT_VERSION}");
    return version;
  }
} // namespace kernel
} // namespace meevax
