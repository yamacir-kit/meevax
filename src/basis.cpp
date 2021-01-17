#include <meevax/kernel/basis.hpp>

/* ---- Embedded Source Codes --------------------------------------------------
 *
 *  basis/hoge.ss
 *
 *  NOTE:
 *    readelf -a hoge.ss.o
 *
 * -------------------------------------------------------------------------- */

extern const char _binary_overture_ss_start[];
extern const char _binary_overture_ss_size[];

extern const char _binary_srfi_1_ss_start[];
extern const char _binary_srfi_1_ss_size[];

namespace meevax
{
inline namespace kernel
{
  string_view const srfi_1   { _binary_srfi_1_ss_start  , reinterpret_cast<std::size_t>(_binary_srfi_1_ss_size  ) };

  string_view const overture { _binary_overture_ss_start, reinterpret_cast<std::size_t>(_binary_overture_ss_size) };
} // namespace kernel
} // namespace meevax
