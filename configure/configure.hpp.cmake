#ifndef INCLUDED_MEEVAX_CONFIGURE_HPP
#define INCLUDED_MEEVAX_CONFIGURE_HPP

#include <string>

#include <meevax/system/list.hpp>
#include <meevax/system/numerical.hpp>
#include <meevax/system/path.hpp>

namespace meevax::system
{
  struct configure
  {
    static inline const auto version_major {make<real>(${PROJECT_VERSION_MAJOR})};
    static inline const auto version_minor {make<real>(${PROJECT_VERSION_MINOR})};
    static inline const auto version_patch {make<real>(${PROJECT_VERSION_PATCH})};

    static inline const auto version {list(
      version_major, version_minor, version_patch
    )};

    static inline const std::string build_date {"${${PROJECT_NAME}_BUILD_DATE}"};
    static inline const std::string build_hash {"${${PROJECT_NAME}_BUILD_HASH}"};
    static inline const std::string build_type {"${CMAKE_BUILD_TYPE}"};

    static inline const auto install_prefix {make<path>(
      "${CMAKE_INSTALL_PREFIX}"
    )};
  };
} // namespace meevax

#endif // INCLUDED_MEEVAX_CONFIGURE_HPP

