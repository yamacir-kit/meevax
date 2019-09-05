#ifndef INCLUDED_MEEVAX_CONFIGURATION_HPP
#define INCLUDED_MEEVAX_CONFIGURATION_HPP

#include <string>

#include <meevax/system/list.hpp>
#include <meevax/system/numerical.hpp>
#include <meevax/system/path.hpp>

namespace meevax::system
{
  struct configuration
  {
    static inline const auto version_major {make<real>(0)};
    static inline const auto version_minor {make<real>(1)};
    static inline const auto version_patch {make<real>(695)};

    static inline const auto version {list(
      version_major, version_minor, version_patch
    )};

    static inline const std::string build_date {"2019/09/06 06:15:00"};
    static inline const std::string build_hash {"0f83b26f6c4a1156c010c16d6d448437b8e633e6"};
    static inline const std::string build_type {"debug"};

    static inline const auto install_prefix {make<path>(
      "/usr/local"
    )};
  };
} // namespace meevax

#endif // INCLUDED_MEEVAX_CONFIGURATION_HPP

