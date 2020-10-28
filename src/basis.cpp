#include <meevax/kernel/basis.hpp>

/* ---- Embedded Source Codes --------------------------------------------------
 *
 *  basis/hoge.ss
 *
 *  NOTE:
 *    readelf -a hoge.ss.o
 *
 * -------------------------------------------------------------------------- */

extern char _binary_overture_ss_start;
extern char _binary_overture_ss_end;

namespace meevax { inline namespace kernel
{
  const string_view overture
  {
                                                        &_binary_overture_ss_start,
    static_cast<std::size_t>(&_binary_overture_ss_end - &_binary_overture_ss_start)
  };
}} // namespace meevax::kernel
