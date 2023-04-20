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
  script const basis = R"###(
${${PROJECT_NAME}_BASIS_meevax.ss}
${${PROJECT_NAME}_BASIS_r4rs.ss}
${${PROJECT_NAME}_BASIS_r4rs-essential.ss}
${${PROJECT_NAME}_BASIS_r5rs.ss}
${${PROJECT_NAME}_BASIS_r7rs.ss}
${${PROJECT_NAME}_BASIS_srfi-1.ss}
${${PROJECT_NAME}_BASIS_srfi-6.ss}
${${PROJECT_NAME}_BASIS_srfi-8.ss}
${${PROJECT_NAME}_BASIS_srfi-9.ss}
${${PROJECT_NAME}_BASIS_srfi-11.ss}
${${PROJECT_NAME}_BASIS_srfi-23.ss}
${${PROJECT_NAME}_BASIS_srfi-34.ss}
${${PROJECT_NAME}_BASIS_srfi-38.ss}
${${PROJECT_NAME}_BASIS_srfi-39.ss}
${${PROJECT_NAME}_BASIS_srfi-45.ss}
${${PROJECT_NAME}_BASIS_srfi-78.ss}
${${PROJECT_NAME}_BASIS_srfi-149.ss}
)###";
}
}
