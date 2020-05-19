#ifndef INCLUDED_MEEVAX_KERNEL_FEATURE_HPP
#define INCLUDED_MEEVAX_KERNEL_FEATURE_HPP

#include <boost/algorithm/string.hpp>

#include <meevax/kernel/list.hpp>
#include <meevax/kernel/numerical.hpp>
#include <meevax/kernel/path.hpp>
#include <meevax/kernel/symbol.hpp>

namespace meevax::kernel
{
  struct feature
    : public object
  {
    static inline const std::string build_type_origin { "${CMAKE_BUILD_TYPE}" };

    static inline const auto build_date { make<symbol>("${${PROJECT_NAME}_BUILD_DATE}") };
    static inline const auto build_hash { make<symbol>("${${PROJECT_NAME}_BUILD_HASH}") };
    static inline const auto build_type { make<symbol>(boost::to_lower_copy(build_type_origin)) };

    static inline const auto cxx_compiler { make<symbol>("${CMAKE_CXX_COMPILER}") };
    static inline const auto cxx_flags    { make<symbol>("${CMAKE_CXX_FLAGS}") };
    static inline const auto cxx_standard { make<real>("${CMAKE_CXX_STANDARD}") };

    static inline const std::string system_name_origin { "${CMAKE_SYSTEM_NAME}" };
    static inline const auto system_name      { make<symbol>(boost::to_lower_copy(system_name_origin)) };
    static inline const auto system_processor { make<symbol>("${CMAKE_SYSTEM_PROCESSOR}") };

    static inline const auto install_prefix { make<path>("${CMAKE_INSTALL_PREFIX}") };

    static inline const auto implementation_name              { make<symbol>("${PROJECT_NAME}") };
    static inline const auto implementation_name_with_version { make<symbol>("${PROJECT_NAME}-${PROJECT_VERSION}") };

    static inline const auto srfi_10 { make<symbol>("srfi-10") };
    static inline const auto srfi_62 { make<symbol>("srfi-62") };

    explicit feature()
      : object
        {
          list(
            implementation_name,
            implementation_name_with_version,
            system_name,
            system_processor,
            srfi_10,
            srfi_62
            )
        }
    {}
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_FEATURE_HPP

