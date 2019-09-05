#ifndef INCLUDED_MEEVAX_CONFIGURATION_HPP
#define INCLUDED_MEEVAX_CONFIGURATION_HPP

#include <meevax/system/list.hpp>
#include <meevax/system/numerical.hpp>
#include <meevax/system/path.hpp>

namespace meevax::system
{
  struct configuration
  {
    static inline const auto version_major {make<real>(0)};
    static inline const auto version_minor {make<real>(1)};
    static inline const auto version_patch {make<real>(696)};

    static inline const auto version {list(
      version_major, version_minor, version_patch
    )};

    static inline const std::string build_date {"2019/09/06 06:28:53"};
    static inline const std::string build_hash {"108b10e7034c6ebf98ef6cdf4bb533932e3c7333"};
    static inline const std::string build_type {"debug"};

    static inline const auto install_prefix {make<path>(
      "/usr/local"
    )};
  };
} // namespace meevax

#endif // INCLUDED_MEEVAX_CONFIGURATION_HPP

