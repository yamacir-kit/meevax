/*
   Copyright 2018-2021 Tatsuya Yamasaki.

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
  auto gmp_version() -> const_reference
  {
    let static const version = make<symbol>(::gmp_version);
    return version;
  }

  auto version() -> const_reference
  {
    let static const version = make<symbol>("${PROJECT_VERSION}");
    return version;
  }

  auto major_version() -> const_reference
  {
    let static const version = make<exact_integer>("${PROJECT_VERSION_MAJOR}");
    return version;
  }

  auto minor_version() -> const_reference
  {
    let static const version = make<exact_integer>("${PROJECT_VERSION_MINOR}");
    return version;
  }

  auto patch_version() -> const_reference
  {
    let static const version = make<exact_integer>("${PROJECT_VERSION_PATCH}");
    return version;
  }

  auto exact_version() -> const_reference
  {
    let static const version = make<symbol>("${${PROJECT_NAME}_VERSION_EXACT}");
    return version;
  }

  auto features() -> const_reference
  {
    let static const features = list(
      // STANDARD FEATURE IDENTIFIERS
      make<symbol>("r4rs"),
      make<symbol>("exact-closed"),
      // make<symbol>("exact-complex"),
      make<symbol>("ieee-float"),
      // make<symbol>("full-unicode"),
      make<symbol>("ratios"),
      make<symbol>("posix"),
      make<symbol>("${CMAKE_SYSTEM_NAME}"),
      make<symbol>("${CMAKE_SYSTEM_PROCESSOR}"),
      // TODO C memory model flags.
      make<symbol>("${${PROJECT_NAME}_BYTE_ORDER}"),
      make<symbol>("${PROJECT_NAME}"), // The name of this implementation.
      make<symbol>("${PROJECT_NAME}-${PROJECT_VERSION}"), // The name and version of this implementation.

      // SUPPORTED SRFIS
      make<symbol>("srfi-5"),
      make<symbol>("srfi-6"),
      make<symbol>("srfi-8"),
      make<symbol>("srfi-10"),
      make<symbol>("srfi-62"),
      make<symbol>("srfi-87"),

      // SUPPORTED OPTIMIZATIONS
      make<symbol>("tail-call-optimization")
      );

    return features;
  }
} // namespace kernel
} // namespace meevax
