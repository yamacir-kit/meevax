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
  string_view const r4rs           = R"###(${${PROJECT_NAME}_BASIS_r4rs.ss})###";
  string_view const r4rs_essential = R"###(${${PROJECT_NAME}_BASIS_r4rs-essential.ss})###";
  string_view const r5rs           = R"###(${${PROJECT_NAME}_BASIS_r5rs.ss})###";
  string_view const r7rs           = R"###(${${PROJECT_NAME}_BASIS_r7rs.ss})###";
  string_view const srfi_1         = R"###(${${PROJECT_NAME}_BASIS_srfi-1.ss})###";
  string_view const srfi_6         = R"###(${${PROJECT_NAME}_BASIS_srfi-6.ss})###";
  string_view const srfi_8         = R"###(${${PROJECT_NAME}_BASIS_srfi-8.ss})###";
  string_view const srfi_9         = R"###(${${PROJECT_NAME}_BASIS_srfi-9.ss})###";
  string_view const srfi_11        = R"###(${${PROJECT_NAME}_BASIS_srfi-11.ss})###";
  string_view const srfi_23        = R"###(${${PROJECT_NAME}_BASIS_srfi-23.ss})###";
  string_view const srfi_34        = R"###(${${PROJECT_NAME}_BASIS_srfi-34.ss})###";
  string_view const srfi_38        = R"###(${${PROJECT_NAME}_BASIS_srfi-38.ss})###";
  string_view const srfi_39        = R"###(${${PROJECT_NAME}_BASIS_srfi-39.ss})###";
  string_view const srfi_45        = R"###(${${PROJECT_NAME}_BASIS_srfi-45.ss})###";
  string_view const srfi_78        = R"###(${${PROJECT_NAME}_BASIS_srfi-78.ss})###";
  string_view const srfi_149       = R"###(${${PROJECT_NAME}_BASIS_srfi-149.ss})###";
  string_view const srfi_211       = R"###(${${PROJECT_NAME}_BASIS_srfi-211.ss})###";
}
}
