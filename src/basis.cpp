#include <meevax/kernel/basis.hpp>

/* ---- Embedded Source Codes --------------------------------------------------
 *
 *  basis/hoge.ss
 *
 *  NOTE:
 *    readelf -a hoge.ss.o
 *
 * -------------------------------------------------------------------------- */

#define DECLARE_BINARY(FILENAME) \
extern const char _binary_##FILENAME##_ss_start[]; \
extern const char _binary_##FILENAME##_ss_end[]; \
extern const char _binary_##FILENAME##_ss_size[]

DECLARE_BINARY(dynamic_wind);
DECLARE_BINARY(srfi_1);
DECLARE_BINARY(overture);

namespace meevax
{
inline namespace kernel
{
  #define DEFINE_BINARY(FILENAME) \
  string_view const FILENAME { \
    _binary_##FILENAME##_ss_start, \
    reinterpret_cast<std::size_t>(_binary_##FILENAME##_ss_size) \
  }

  DEFINE_BINARY(dynamic_wind);
  DEFINE_BINARY(srfi_1);
  DEFINE_BINARY(overture);
} // namespace kernel
} // namespace meevax
