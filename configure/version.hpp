#ifndef INCLUDED_MEEVAX_KERNEL_VERSION_HPP
#define INCLUDED_MEEVAX_KERNEL_VERSION_HPP

#include <boost/lexical_cast.hpp>
#include <boost/version.hpp>

#include <meevax/kernel/list.hpp>
#include <meevax/kernel/number.hpp>
#include <meevax/kernel/symbol.hpp>
#include <meevax/utility/construct_on_first_use.hpp>

#ifdef major // NOTE: Maybe defined by <sys/sysmacros.h> when _GNU_SOURCE.
#undef major
#endif

#ifdef minor // NOTE: Maybe defined by <sys/sysmacros.h> when _GNU_SOURCE.
#undef minor
#endif

namespace meevax
{
inline namespace kernel
{
  constexpr auto boost_version = []() -> bytestring
  {
    static constexpr std::size_t major { BOOST_VERSION / 100000 };
    static constexpr std::size_t minor { BOOST_VERSION / 100 % 1000 };
    static constexpr std::size_t patch { BOOST_VERSION % 100 };

    static const auto semantic {
      boost::lexical_cast<bytestring>(major) + "." +
      boost::lexical_cast<bytestring>(minor) + "." +
      boost::lexical_cast<bytestring>(patch)
    };

    return semantic;
  };

  struct version
    : public object
  {
    #define boilerplate(NAME, ...) \
    auto NAME() const -> decltype(auto) \
    { \
      static const auto x { __VA_ARGS__ }; \
      return x; \
    } static_assert(true)

    boilerplate(major, make<exact_integer>("${PROJECT_VERSION_MAJOR}"));
    boilerplate(minor, make<exact_integer>("${PROJECT_VERSION_MINOR}"));
    boilerplate(patch, make<exact_integer>("${PROJECT_VERSION_PATCH}"));

    boilerplate(semantic, make<symbol>("${PROJECT_VERSION}"));

    boilerplate(libraries,
      static_cast<object>(
        list(
          cons(
            make<symbol>("boost"),
            make<symbol>(boost_version())),
          cons(
            make<symbol>("gmp"),
            make<symbol>(gmp_version))
          )));

    #undef boilerplate

    explicit version()
      : object { list(major(), minor(), patch()) }
    {}
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_CONFIGURE_VERSION_HPP
