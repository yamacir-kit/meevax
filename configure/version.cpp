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
  auto version() -> object const&
  {
    let static const version = string_to_symbol("${PROJECT_VERSION}");
    return version;
  }

  auto major_version() -> object const&
  {
    let static const version = make<exact_integer>("${PROJECT_VERSION_MAJOR}");
    return version;
  }

  auto minor_version() -> object const&
  {
    let static const version = make<exact_integer>("${PROJECT_VERSION_MINOR}");
    return version;
  }

  auto patch_version() -> object const&
  {
    let static const version = make<exact_integer>("${PROJECT_VERSION_PATCH}");
    return version;
  }

  auto exact_version() -> object const&
  {
    let static const version = string_to_symbol("${${PROJECT_NAME}_VERSION_EXACT}");
    return version;
  }

  auto features() -> object const&
  {
    let static const features = list(
      string_to_symbol("r5rs"),
      string_to_symbol("exact-closed"),
      string_to_symbol("exact-complex"),
      string_to_symbol("ieee-float"),
      string_to_symbol("ratios"),
      string_to_symbol("posix"),
      string_to_symbol("${CMAKE_SYSTEM_NAME}"),
      string_to_symbol("${CMAKE_SYSTEM_PROCESSOR}"),
      // TODO C memory model flags.
      string_to_symbol("${${PROJECT_NAME}_BYTE_ORDER}"),
      string_to_symbol("${PROJECT_NAME}"), // The name of this implementation.
      string_to_symbol("${PROJECT_NAME}-${PROJECT_VERSION}") // The name and version of this implementation.
      );

    return features;
  }

  auto help() -> std::string const&
  {
    std::string static const help = R"(${${PROJECT_NAME}_HELP_TEXT})";
    return help;
  }
} // namespace kernel
} // namespace meevax
