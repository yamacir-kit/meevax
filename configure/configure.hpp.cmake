#ifndef INCLUDED_MEEVAX_CONFIGURE_HPP
#define INCLUDED_MEEVAX_CONFIGURE_HPP

#include <meevax/system/numerical.hpp>
#include <meevax/system/srfi-1.hpp>

namespace meevax
{
  static const auto version_major {system::make<system::real>(${PROJECT_VERSION_MAJOR})};
  static const auto version_minor {system::make<system::real>(${PROJECT_VERSION_MINOR})};
  static const auto version_patch {system::make<system::real>(${PROJECT_VERSION_PATCH})};

  static const auto version {system::list(version_major, version_minor, version_patch)};

  static const std::string build_date {"${${PROJECT_NAME}_BUILD_DATE}"};
  static const std::string build_hash {"${${PROJECT_NAME}_BUILD_HASH}"};
  static const std::string build_type {"${CMAKE_BUILD_TYPE}"};
} // namespace meevax

#endif // INCLUDED_MEEVAX_CONFIGURE_HPP

