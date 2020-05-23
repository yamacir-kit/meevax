#ifndef INCLUDED_MEEVAX_KERNEL_FEATURE_HPP
#define INCLUDED_MEEVAX_KERNEL_FEATURE_HPP

#include <meevax/kernel/list.hpp>
#include <meevax/kernel/numerical.hpp>
#include <meevax/kernel/path.hpp>
#include <meevax/kernel/symbol.hpp>
#include <meevax/utility/construct_on_first_use.hpp>

namespace meevax::kernel
{
  struct feature
    : public object
  {
    Construct_On_First_Use(build_date, make<symbol>("${${PROJECT_NAME}_BUILD_DATE}"));
    Construct_On_First_Use(build_hash, make<symbol>("${${PROJECT_NAME}_BUILD_HASH}"));
    Construct_On_First_Use(build_type, make<symbol>("${CMAKE_BUILD_TYPE}"));

    Construct_On_First_Use(cxx_compiler, make<symbol>("${CMAKE_CXX_COMPILER}"));
    Construct_On_First_Use(cxx_flags, make<symbol>("${CMAKE_CXX_FLAGS}"));
    Construct_On_First_Use(cxx_standard, make<real>("${CMAKE_CXX_STANDARD}"));

    Construct_On_First_Use(system_name, make<symbol>("${CMAKE_SYSTEM_NAME}"));
    Construct_On_First_Use(system_processor, make<symbol>("${CMAKE_SYSTEM_PROCESSOR}"));

    Construct_On_First_Use(install_prefix, make<path>("${CMAKE_INSTALL_PREFIX}"));

    Construct_On_First_Use(implementation_name, make<symbol>("${PROJECT_NAME}"));
    Construct_On_First_Use(implementation_name_with_version, make<symbol>("${PROJECT_NAME}-${PROJECT_VERSION}"));

    Construct_On_First_Use(srfi_10, make<symbol>("srfi-10"));
    Construct_On_First_Use(srfi_62, make<symbol>("srfi-62"));

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
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_FEATURE_HPP

