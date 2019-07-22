#ifndef INCLUDED_MEEVAX_CONFIGURE_HPP
#define INCLUDED_MEEVAX_CONFIGURE_HPP

#include <string>

namespace meevax
{
  static const std::string version_major {"${PROJECT_VERSION_MAJOR}"};
  static const std::string version_minor {"${PROJECT_VERSION_MINOR}"};
  static const std::string version_patch {"${PROJECT_VERSION_PATCH}"};
  static const std::string version       {"${PROJECT_VERSION}"};

  static const std::string build_date {"${${PROJECT_NAME}_BUILD_DATE}"};
  static const std::string build_hash {"${${PROJECT_NAME}_BUILD_HASH}"};
  static const std::string build_type {"${CMAKE_BUILD_TYPE}"};
} // namespace meevax

#endif // INCLUDED_MEEVAX_CONFIGURE_HPP

