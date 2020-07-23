#ifndef INCLUDED_MEEVAX_KERNEL_LIBRARY_HPP
#define INCLUDED_MEEVAX_KERNEL_LIBRARY_HPP

#include <string_view>

#if __cpp_lib_string_view
#define STRING_VIEW std::string_view
#else
#define STRING_VIEW std::experimental::string_view
#endif

extern const STRING_VIEW overture;
// {
//                                                       &_binary_overture_ss_start,
//   static_cast<std::size_t>(&_binary_overture_ss_end - &_binary_overture_ss_start)
// };

#endif // INCLUDED_MEEVAX_KERNEL_LIBRARY_HPP
