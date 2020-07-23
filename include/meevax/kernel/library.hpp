#ifndef INCLUDED_MEEVAX_KERNEL_LIBRARY_HPP
#define INCLUDED_MEEVAX_KERNEL_LIBRARY_HPP

#include <string_view>

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

static const std::
#if not __cpp_lib_string_view
experimental::
#endif
string_view overture
{
  &_binary_overture_ss_start,
  static_cast<std::size_t>(&_binary_overture_ss_end - &_binary_overture_ss_start)
};

#endif // INCLUDED_MEEVAX_KERNEL_LIBRARY_HPP
