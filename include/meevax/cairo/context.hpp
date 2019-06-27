#ifndef INCLUDED_MEEVAX_CAIRO_CONTEXT_HPP
#define INCLUDED_MEEVAX_CAIRO_CONTEXT_HPP

#include <memory>
#include <utility>

#include <cairo/cairo.h>

#include <meevax/cairo/surface.hpp>

namespace meevax::cairo
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

    #define CAIRO_SIMPLE_CONTEXT_OPERATOR(NAME)                                \
    template <typename... Ts>                                                  \
    decltype(auto) NAME(Ts&&... xs) noexcept                                   \
    {                                                                          \
      return cairo_##NAME(*this, std::forward<Ts>(xs)...);                     \
    }

    CAIRO_SIMPLE_CONTEXT_OPERATOR(set_source_rgb)
    CAIRO_SIMPLE_CONTEXT_OPERATOR(paint)

    #undef CAIRO_SIMPLE_CONTEXT_OPERATOR
  };
} // namespace meevax::cairo

#endif // INCLUDED_MEEVAX_CAIRO_CONTEXT_HPP

