#ifndef INCLUDED_MEEVAX_KERNEL_VERSION_HPP
#define INCLUDED_MEEVAX_KERNEL_VERSION_HPP

#include <boost/lexical_cast.hpp>
#include <boost/version.hpp>

#include <meevax/kernel/list.hpp>
#include <meevax/kernel/number.hpp>
#include <meevax/kernel/symbol.hpp>

namespace meevax
{
inline namespace kernel
{
  let const& boost_version()
  {
    static constexpr std::size_t major = BOOST_VERSION / 100000;
    static constexpr std::size_t minor = BOOST_VERSION / 100 % 1000;
    static constexpr std::size_t patch = BOOST_VERSION % 100;

    let static const version = make<symbol>(boost::lexical_cast<bytestring>(major) + "." +
                                            boost::lexical_cast<bytestring>(minor) + "." +
                                            boost::lexical_cast<bytestring>(patch));
    return version;
  }

  let const& gmp_version()
  {
    let static const version = make<symbol>(::gmp_version);

    return version;
  }

  let const& major_version()
  {
    let static const major = make<exact_integer>("${PROJECT_VERSION_MAJOR}");

    return major;
  }

  let const& minor_version()
  {
    let static const minor = make<exact_integer>("${PROJECT_VERSION_MINOR}");

    return minor;
  }

  let const& patch_version()
  {
    let static const patch = make<exact_integer>("${PROJECT_VERSION_PATCH}");

    return patch;
  }

  let const& version()
  {
    let static const version = make<symbol>("${PROJECT_VERSION}");

    return version;
  }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_CONFIGURE_VERSION_HPP
