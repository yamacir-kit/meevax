#ifndef INCLUDED_MEEVAX_KERNEL_FEATURE_HPP
#define INCLUDED_MEEVAX_KERNEL_FEATURE_HPP

#include <meevax/kernel/list.hpp>
#include <meevax/kernel/number.hpp>
#include <meevax/kernel/path.hpp>
#include <meevax/kernel/symbol.hpp>

namespace meevax
{
inline namespace kernel
{
  let const& features()
  {
    let static const features = list(

      // make<symbol>("r7rs"), // All R7RS Scheme implementations have this feature.
      make<symbol>("exact-closed"), // All algebraic operations except / produce exact values given exact inputs.
      // make<symbol>("exact-complex"), // Exact complex numbers are provided.
      make<symbol>("ieee-float"), // Inexact numbers are IEEE 754 binary floating point values.
      // make<symbol>("full-unicode"), // All Unicode characters present in Unicode version 6.0 are supported as Scheme characters.
      make<symbol>("ratios"), // / with exact arguments produces an exact result when the divisor is nonzero.
      make<symbol>("posix"), // This implementation is running on a POSIX system.
      // make<symbol>("windows"), // This implementation is running on Windows.
      make<symbol>("${CMAKE_SYSTEM_NAME}"), // Operating system flags (perhaps more than one).
      make<symbol>("${CMAKE_SYSTEM_PROCESSOR}"), // CPU architecture flags.
      // TODO C memory model flags.
      // TODO Byte order flags.

      make<symbol>("${PROJECT_NAME}"), // The name of this implementation.
      make<symbol>("${PROJECT_NAME}-${PROJECT_VERSION}"), // The name and version of this implementation.

      make<symbol>("srfi-5"),
      make<symbol>("srfi-6"),
      make<symbol>("srfi-10"),
      make<symbol>("srfi-62"),
      make<symbol>("srfi-87")
      );

    return features;
  }

  let const& install_prefix()
  {
    let static const prefix = make<path>("${CMAKE_INSTALL_PREFIX}");

    return prefix;
  }

  let const& build_date()
  {
    let static const result = make<symbol>("${${PROJECT_NAME}_BUILD_DATE}");

    return result;
  }

  let const& build_hash()
  {
    let static const result = make<symbol>("${${PROJECT_NAME}_BUILD_HASH}");

    return result;
  }

  let const& build_type()
  {
    let static const result = make<symbol>("${CMAKE_BUILD_TYPE}");

    return result;
  }

  let const& cxx_compiler()
  {
    let static const result = make<symbol>("${CMAKE_CXX_COMPILER}");

    return result;
  }

  let const& cxx_flags()
  {
    let static const result = make<symbol>("${CMAKE_CXX_FLAGS}");

    return result;
  }

  let const& cxx_standard()
  {
    let static const result = make<exact_integer>("${CMAKE_CXX_STANDARD}");

    return result;
  }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_FEATURE_HPP
