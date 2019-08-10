#ifndef INCLUDED_MEEVAX_CONFIGURE_HPP
#define INCLUDED_MEEVAX_CONFIGURE_HPP

#include <string>

namespace meevax::system
{
  struct configure
  {
    static inline const std::string version_major {"${PROJECT_VERSION_MAJOR}"};
    static inline const std::string version_minor {"${PROJECT_VERSION_MINOR}"};
    static inline const std::string version_patch {"${PROJECT_VERSION_PATCH}"};

    static inline const std::string version {"${PROJECT_VERSION}"};

    static inline const std::string build_date {"${${PROJECT_NAME}_BUILD_DATE}"};
    static inline const std::string build_hash {"${${PROJECT_NAME}_BUILD_HASH}"};
    static inline const std::string build_type {"${CMAKE_BUILD_TYPE}"};

    static inline const std::string install_prefix {"${CMAKE_INSTALL_PREFIX}"};
  };
} // namespace meevax

#endif // INCLUDED_MEEVAX_CONFIGURE_HPP

