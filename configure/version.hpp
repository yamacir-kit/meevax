#ifndef INCLUDED_MEEVAX_KERNEL_VERSION_HPP
#define INCLUDED_MEEVAX_KERNEL_VERSION_HPP

#include <meevax/kernel/exact_integer.hpp>
#include <meevax/kernel/list.hpp>
#include <meevax/kernel/symbol.hpp>

namespace meevax
{
inline namespace kernel
{
  [[deprecated]]
  auto gmp_version() -> pair::const_reference
  {
    let static const version = make<symbol>(::gmp_version);
    return version;
  }

  auto version() -> pair::const_reference
  {
    let static const version = make<symbol>("${PROJECT_VERSION}");
    return version;
  }

  auto major_version() -> pair::const_reference
  {
    let static const version = make<exact_integer>("${PROJECT_VERSION_MAJOR}");
    return version;
  }

  auto minor_version() -> pair::const_reference
  {
    let static const version = make<exact_integer>("${PROJECT_VERSION_MINOR}");
    return version;
  }

  auto patch_version() -> pair::const_reference
  {
    let static const version = make<exact_integer>("${PROJECT_VERSION_PATCH}");
    return version;
  }

  auto exact_version() -> pair::const_reference
  {
    let static const version = make<symbol>("${${PROJECT_NAME}_VERSION_EXACT}");
    return version;
  }

  auto features() -> pair::const_reference
  {
    let static const features = list(
      // STANDARD FEATURE IDENTIFIERS
      make<symbol>("r4rs"),
      make<symbol>("exact-closed"),
      // make<symbol>("exact-complex"),
      make<symbol>("ieee-float"),
      // make<symbol>("full-unicode"),
      make<symbol>("ratios"),
      make<symbol>("posix"),
      make<symbol>("${CMAKE_SYSTEM_NAME}"),
      make<symbol>("${CMAKE_SYSTEM_PROCESSOR}"),
      // TODO C memory model flags.
      make<symbol>("${${PROJECT_NAME}_BYTE_ORDER}"),
      make<symbol>("${PROJECT_NAME}"),
      make<symbol>("${PROJECT_NAME}-${PROJECT_VERSION}"),

      // SUPPORTED SRFIS
      make<symbol>("srfi-5"),
      make<symbol>("srfi-6"),
      make<symbol>("srfi-8"),
      make<symbol>("srfi-10"),
      make<symbol>("srfi-62"),
      make<symbol>("srfi-87"),

      // SUPPORTED OPTIMIZATIONS
      make<symbol>("tail-call-optimization")
      );

    return features;
  }

  // auto install_prefix() -> pair::const_reference
  // {
  //   let static const prefix = make<path>("${CMAKE_INSTALL_PREFIX}");
  //
  //   return prefix;
  // }
  //
  // auto build_date() -> pair::const_reference
  // {
  //   let static const result = make<symbol>("${${PROJECT_NAME}_BUILD_DATE}");
  //
  //   return result;
  // }
  //
  // auto build_type() -> pair::const_reference
  // {
  //   let static const result = make<symbol>("${CMAKE_BUILD_TYPE}");
  //
  //   return result;
  // }
  //
  // auto cxx_compiler() -> pair::const_reference
  // {
  //   let static const result = make<symbol>("${CMAKE_CXX_COMPILER}");
  //
  //   return result;
  // }
  //
  // auto cxx_flags() -> pair::const_reference
  // {
  //   let static const result = make<symbol>("${CMAKE_CXX_FLAGS}");
  //
  //   return result;
  // }
  //
  // auto cxx_standard() -> pair::const_reference
  // {
  //   let static const result = make<exact_integer>("${CMAKE_CXX_STANDARD}");
  //
  //   return result;
  // }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_CONFIGURE_VERSION_HPP
