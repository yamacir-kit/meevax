#ifndef INCLUDED_MEEVAX_KERNEL_NUMERICAL_TYPES_HPP
#define INCLUDED_MEEVAX_KERNEL_NUMERICAL_TYPES_HPP

#include <meevax/kernel/pair.hpp>

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

  using default_float = floating_point<decltype(0.0)>;

  struct ratio;

  struct exact_integer;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_NUMERICAL_TYPES_HPP
