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

#include <meevax/kernel/basis.hpp>

/* ---- Embedded Source Codes --------------------------------------------------
 *
 *  basis/hoge.ss
 *
 *  NOTE: readelf -a hoge.ss.o
 *
 * -------------------------------------------------------------------------- */

#define DEFINE_BINARY(FILENAME)                                                \
                                                                               \
extern "C" const char _binary_##FILENAME##_ss_start[];                         \
extern "C" const char _binary_##FILENAME##_ss_end[];                           \
extern "C" const char _binary_##FILENAME##_ss_size[];                          \
                                                                               \
namespace meevax                                                               \
{                                                                              \
inline namespace kernel                                                        \
{                                                                              \
  string_view const FILENAME {                                                 \
    _binary_##FILENAME##_ss_start,                                             \
    static_cast<std::size_t>(                                                  \
      _binary_##FILENAME##_ss_end - _binary_##FILENAME##_ss_start)             \
  };                                                                           \
}                                                                              \
} static_assert(true)

DEFINE_BINARY(overture);
DEFINE_BINARY(r7rs);
DEFINE_BINARY(srfi_1);
DEFINE_BINARY(srfi_8);
DEFINE_BINARY(srfi_23);
DEFINE_BINARY(srfi_34);
DEFINE_BINARY(srfi_39);
DEFINE_BINARY(srfi_45);
DEFINE_BINARY(srfi_78);
DEFINE_BINARY(srfi_149);
