#ifndef INCLUDED_MEEVAX_KERNEL_FEATURE_HPP
#define INCLUDED_MEEVAX_KERNEL_FEATURE_HPP

#include <meevax/kernel/list.hpp>
#include <meevax/kernel/symbol.hpp>

namespace meevax::kernel
{
  struct feature
    : public object
  {
    static inline const auto build_date { make<symbol>("${${PROJECT_NAME}_BUILD_DATE}") };
    static inline const auto build_hash { make<symbol>("${${PROJECT_NAME}_BUILD_HASH}") };
    static inline const auto build_type { make<symbol>("${CMAKE_BUILD_TYPE}") };

    static inline const auto compiler { make<symbol>("${CMAKE_CXX_COMPILER}") };
    static inline const auto flags    { make<symbol>("${CMAKE_CXX_FLAGS}") };

    static inline const auto system_name { make<symbol>("${CMAKE_SYSTEM_NAME}") };

    static inline const auto implementation_name              { make<symbol>("${PROJECT_NAME}") };
    static inline const auto implementation_name_with_version { make<symbol>("${PROJECT_NAME}-${PROJECT_VERSION}") };

    explicit feature()
      : object
        {
          list(
            implementation_name,
            implementation_name_with_version,
            system_name)
        }
    {}
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_FEATURE_HPP

