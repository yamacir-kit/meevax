#ifndef INCLUDED_MEEVAX_KERNEL_VERSION_HPP
#define INCLUDED_MEEVAX_KERNEL_VERSION_HPP

#include <boost/lexical_cast.hpp>
#include <boost/version.hpp>

#include <meevax/kernel/list.hpp>
#include <meevax/kernel/numerical.hpp>
#include <meevax/kernel/symbol.hpp>

namespace meevax::kernel
{
  static auto boost_version() -> std::string
  {
    static constexpr std::size_t major { BOOST_VERSION / 100000 };
    static constexpr std::size_t minor { BOOST_VERSION / 100 % 1000 };
    static constexpr std::size_t patch { BOOST_VERSION % 100 };

    return boost::lexical_cast<std::string>(major)
         + "."
         + boost::lexical_cast<std::string>(minor)
         + "."
         + boost::lexical_cast<std::string>(patch);
  }

  struct version
    : public object
  {
    static inline const auto major { make<real>("${PROJECT_VERSION_MAJOR}") };
    static inline const auto minor { make<real>("${PROJECT_VERSION_MINOR}") };
    static inline const auto patch { make<real>("${PROJECT_VERSION_PATCH}") };

    static inline const auto semantic { make<symbol>("${PROJECT_VERSION}") };

    static inline const object libraries {
      list(
        cons(
          make<symbol>("boost"),
          make<symbol>(boost_version())),
        cons(
          make<symbol>("gmp"),
          make<symbol>(gmp_version)),
        cons(
          make<symbol>("mpfr"),
          make<symbol>(mpfr_get_version()))
        )
    };

    explicit version()
      : object { list(major, minor, patch) }
    {}
  };
} // namespace meevax::kernal

#endif // INCLUDED_MEEVAX_CONFIGURE_VERSION_HPP

