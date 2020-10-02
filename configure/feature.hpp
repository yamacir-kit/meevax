#ifndef INCLUDED_MEEVAX_KERNEL_FEATURE_HPP
#define INCLUDED_MEEVAX_KERNEL_FEATURE_HPP

#include <meevax/kernel/list.hpp>
#include <meevax/kernel/number.hpp>
#include <meevax/kernel/path.hpp>
#include <meevax/kernel/symbol.hpp>

namespace meevax { inline namespace kernel
{
  struct feature
    : public object
  {
    #define boilerplate(NAME, ...) \
    auto NAME() const -> decltype(auto) \
    { \
      static const auto x { __VA_ARGS__ }; \
      return x; \
    } static_assert(true)

    boilerplate(build_date, make<symbol>("${${PROJECT_NAME}_BUILD_DATE}"));
    boilerplate(build_hash, make<symbol>("${${PROJECT_NAME}_BUILD_HASH}"));
    boilerplate(build_type, make<symbol>("${CMAKE_BUILD_TYPE}"));

    boilerplate(cxx_compiler, make<symbol>("${CMAKE_CXX_COMPILER}"));
    boilerplate(cxx_flags, make<symbol>("${CMAKE_CXX_FLAGS}"));
    boilerplate(cxx_standard, make<exact_integer>("${CMAKE_CXX_STANDARD}"));

    boilerplate(system_name, make<symbol>("${CMAKE_SYSTEM_NAME}"));
    boilerplate(system_processor, make<symbol>("${CMAKE_SYSTEM_PROCESSOR}"));

    boilerplate(install_prefix, make<path>("${CMAKE_INSTALL_PREFIX}"));

    boilerplate(implementation_name, make<symbol>("${PROJECT_NAME}"));
    boilerplate(implementation_name_with_version, make<symbol>("${PROJECT_NAME}-${PROJECT_VERSION}"));

    boilerplate(srfi_10, make<symbol>("srfi-10"));
    boilerplate(srfi_62, make<symbol>("srfi-62"));

    #undef boilerplate

    explicit feature()
      : object
        {
          list(
            implementation_name(),
            implementation_name_with_version(),
            system_name(),
            system_processor(),
            srfi_10(),
            srfi_62()
            )
        }
    {}
  };
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_FEATURE_HPP
