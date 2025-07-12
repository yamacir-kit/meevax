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

#include <meevax/kernel/version.hpp>
#include <meevax/memory/model.hpp>

namespace meevax::inline kernel
{
  auto help() noexcept -> std::string_view
  {
    return R"(${${PROJECT_NAME}_HELP})";
  }

  auto features() -> object const&
  {
    let static const features = list(
      make_symbol("r7rs"),
      make_symbol("exact-closed"),
      make_symbol("exact-complex"),
      make_symbol("ieee-float"),
      make_symbol("full-unicode"),
      make_symbol("ratios"),
      make_symbol("posix"),
      make_symbol("${${PROJECT_NAME}_SYSTEM_NAME}"),
      make_symbol("${CMAKE_SYSTEM_PROCESSOR}"),
      make_symbol(memory::model::name()),
      make_symbol(std::endian::native == std::endian::little ? "little-endian" : "big-endian"),
      make_symbol("${PROJECT_NAME}"),
      make_symbol("${PROJECT_NAME}-${PROJECT_VERSION}")

      #ifdef FP_FAST_FMA
    , make_symbol("FP_FAST_FMA")
      #endif

      #if __cpp_lib_math_special_functions
    , make_symbol("__cpp_lib_math_special_functions")
      #endif
      );

    return features;
  }

  auto version() -> object const&
  {
    let static const version = make_symbol("${PROJECT_VERSION}");
    return version;
  }
} // namespace meevax::kernel
