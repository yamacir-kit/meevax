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

#ifndef INCLUDED_MEEVAX_KERNEL_NUMERIC_TOWER_HPP
#define INCLUDED_MEEVAX_KERNEL_NUMERIC_TOWER_HPP

namespace meevax
{
inline namespace kernel
{
  /* ---- Numeric Tower --------------------------------------------------------
   *
   *  number
   *   `-- complex
   *        `-- real
   *             |-- floating-point (IEEE 754)
   *             |    |-- binary  16
   *             |    |-- binary  32 = single_float
   *             |    |-- binary  64 = double_float
   *             |    `-- binary 128
   *             `-- rational
   *                  |-- ratio
   *                  `-- exact-integer
   *                       |-- multi-precision exact-integer
   *                       `-- fixed precision exact-integer
   *                            |-- signed and unsigned   8
   *                            |-- signed and unsigned  16
   *                            |-- signed and unsigned  32
   *                            |-- signed and unsigned  64
   *                            `-- signed and unsigned 128
   *
   * ------------------------------------------------------------------------ */

  template <typename T>
  struct floating_point;

  using single_float = floating_point<float>;
  using double_float = floating_point<double>;
  using system_float = floating_point<decltype(0.0)>;

  struct ratio;

  struct exact_integer;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_NUMERIC_TOWER_HPP
