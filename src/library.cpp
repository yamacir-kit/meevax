#include <meevax/kernel/library.hpp>

/* ==== Embedded Source Codes ==================================================
*
* library/hoge.ss
*
* NOTE:
*   readelf -a hoge.ss.o
*
*============================================================================ */

extern char _binary_overture_ss_start;
extern char _binary_overture_ss_end;

const STRING_VIEW overture
{
                                                      &_binary_overture_ss_start,
  static_cast<std::size_t>(&_binary_overture_ss_end - &_binary_overture_ss_start)
};
