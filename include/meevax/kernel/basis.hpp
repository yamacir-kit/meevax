#ifndef INCLUDED_MEEVAX_KERNEL_LIBRARY_HPP
#define INCLUDED_MEEVAX_KERNEL_LIBRARY_HPP

#include <string_view>

namespace meevax { inline namespace kernel
{
  #if __cpp_lib_string_view
  using string_view = std::string_view;
  #else
  using string_view = std::experimental::string_view;
  #endif

  extern const string_view overture;
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_LIBRARY_HPP
