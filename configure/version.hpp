#ifndef INCLUDED_MEEVAX_KERNEL_VERSION_HPP
#define INCLUDED_MEEVAX_KERNEL_VERSION_HPP

#include <meevax/kernel/list.hpp>
#include <meevax/kernel/number.hpp>
#include <meevax/kernel/symbol.hpp>

namespace meevax
{
inline namespace kernel
{
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
