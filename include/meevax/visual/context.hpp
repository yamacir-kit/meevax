#ifndef INCLUDED_MEEVAX_VISUAL_CONTEXT_HPP
#define INCLUDED_MEEVAX_VISUAL_CONTEXT_HPP

#include <memory>
#include <utility>

#include <cairo/cairo.h>

#include <meevax/visual/surface.hpp>

namespace meevax::visual
{
  struct context
    : public std::shared_ptr<cairo_t>
  {
    explicit context(const surface& surface)
      : std::shared_ptr<cairo_t> {cairo_create(surface), cairo_destroy}
    {}

    operator element_type*() const noexcept
    {
      return get();
    }

    #define VISUAL_CONTEXT_OPERATOR(NAME)                                      \
    template <typename... Ts>                                                  \
    decltype(auto) NAME(Ts&&... xs) noexcept                                   \
    {                                                                          \
      return cairo_##NAME(*this, std::forward<Ts>(xs)...);                     \
    }

    VISUAL_CONTEXT_OPERATOR(set_source_rgb)
    VISUAL_CONTEXT_OPERATOR(paint)

    #undef VISUAL_CONTEXT_OPERATOR
  };
} // namespace meevax::visual

#endif // INCLUDED_MEEVAX_VISUAL_CONTEXT_HPP

