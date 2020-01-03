#ifndef INCLUDED_MEEVAX_KERNEL_VERSION_HPP
#define INCLUDED_MEEVAX_KERNEL_VERSION_HPP

#include <meevax/kernel/list.hpp>
#include <meevax/kernel/numerical.hpp>
#include <meevax/kernel/symbol.hpp>

namespace meevax::kernel
{
  struct version
    : public object
  {
    static inline const auto major {make<real>("${PROJECT_VERSION_MAJOR}")};
    static inline const auto minor {make<real>("${PROJECT_VERSION_MINOR}")};
    static inline const auto patch {make<real>("${PROJECT_VERSION_PATCH}")};

    static inline const auto semantic {make<symbol>("${PROJECT_VERSION}")};

    explicit version()
      : object {list(major, minor, patch)}
    {}
  };
} // namespace meevax::kernal

#endif // INCLUDED_MEEVAX_CONFIGURE_VERSION_HPP

