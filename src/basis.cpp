#include <meevax/kernel/basis.hpp>

/* ---- Embedded Source Codes --------------------------------------------------
 *
 *  basis/hoge.ss
 *
 *  NOTE:
 *    readelf -a hoge.ss.o
 *
 * -------------------------------------------------------------------------- */

extern const char _binary_overture_ss_start;
extern const char _binary_overture_ss_end;
extern const char _binary_overture_ss_size;

namespace meevax { inline namespace kernel
{
  const string_view overture { &_binary_overture_ss_start, reinterpret_cast<std::size_t>(&_binary_overture_ss_size) };
}} // namespace meevax::kernel
