#include <meevax/kernel/basis.hpp>

/* ---- Embedded Source Codes --------------------------------------------------
 *
 *  basis/hoge.ss
 *
 *  NOTE: readelf -a hoge.ss.o
 *
 * -------------------------------------------------------------------------- */

#define DECLARE_BINARY(FILENAME) \
extern "C" const char _binary_##FILENAME##_ss_start[]; \
extern "C" const char _binary_##FILENAME##_ss_end[]; \
extern "C" const char _binary_##FILENAME##_ss_size[]

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
    static_cast<std::size_t>(_binary_##FILENAME##_ss_end - _binary_##FILENAME##_ss_start) \
  }

  DEFINE_BINARY(dynamic_wind);
  DEFINE_BINARY(srfi_1);
  DEFINE_BINARY(overture);
} // namespace kernel
} // namespace meevax
