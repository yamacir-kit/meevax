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

#include <meevax/kernel/basis.hpp>

namespace meevax
{
inline namespace kernel
{
  std::vector<char const*> const basis
  {
    R"##(${${PROJECT_NAME}_BASIS_meevax.ss})##",
    R"##(${${PROJECT_NAME}_BASIS_r4rs-essential.ss})##",
    R"##(${${PROJECT_NAME}_BASIS_r4rs.ss})##",
    R"##(${${PROJECT_NAME}_BASIS_r5rs.ss})##",
    R"##(${${PROJECT_NAME}_BASIS_r7rs.ss})##",
    R"##(${${PROJECT_NAME}_BASIS_srfi-1.ss})##",
    R"##(${${PROJECT_NAME}_BASIS_srfi-4.ss})##",
    R"##(${${PROJECT_NAME}_BASIS_srfi-6.ss})##",
    R"##(${${PROJECT_NAME}_BASIS_srfi-8.ss})##",
    R"##(${${PROJECT_NAME}_BASIS_srfi-9.ss})##",
    R"##(${${PROJECT_NAME}_BASIS_srfi-11.ss})##",
    R"##(${${PROJECT_NAME}_BASIS_srfi-23.ss})##",
    R"##(${${PROJECT_NAME}_BASIS_srfi-31.ss})##",
    R"##(${${PROJECT_NAME}_BASIS_srfi-34.ss})##",
    R"##(${${PROJECT_NAME}_BASIS_srfi-38.ss})##",
    R"##(${${PROJECT_NAME}_BASIS_srfi-39.ss})##",
    R"##(${${PROJECT_NAME}_BASIS_srfi-45.ss})##",
    R"##(${${PROJECT_NAME}_BASIS_srfi-78.ss})##",
    R"##(${${PROJECT_NAME}_BASIS_srfi-149.ss})##",
  };
}
}
