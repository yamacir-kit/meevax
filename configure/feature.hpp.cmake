#ifndef INCLUDED_MEEVAX_KERNEL_FEATURE_HPP
#define INCLUDED_MEEVAX_KERNEL_FEATURE_HPP

#include <meevax/kernel/list.hpp>
#include <meevax/kernel/symbol.hpp>

namespace meevax::kernel
{
  struct feature
    : public object
  {
    static inline const auto commit {
      make<symbol>("commit-" "${${PROJECT_NAME}_BUILD_HASH}")
    };

    static inline const auto date {
      make<symbol>("${${PROJECT_NAME}_BUILD_DATE}"
    )};

    static inline const auto type {
      make<symbol>("${CMAKE_BUILD_TYPE}"
    )};

    explicit feature()
      : object {list(date, commit, type)}
    {}
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_FEATURE_HPP

