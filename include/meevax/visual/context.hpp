#ifndef INCLUDED_MEEVAX_VISUAL_CONTEXT_HPP
#define INCLUDED_MEEVAX_VISUAL_CONTEXT_HPP

#include <memory>
#include <utility>

#include <cairo/cairo.h>

namespace meevax::visual
{
  struct context
    : public std::shared_ptr<cairo_t>
  {
    template <typename... Ts>
    explicit context(Ts&&... xs)
      : std::shared_ptr<cairo_t> {cairo_create(std::forward<Ts>(xs)...), cairo_destroy}
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

    VISUAL_CONTEXT_OPERATOR(move_to)
    VISUAL_CONTEXT_OPERATOR(paint)
    VISUAL_CONTEXT_OPERATOR(select_font_face)
    VISUAL_CONTEXT_OPERATOR(set_font_size)
    VISUAL_CONTEXT_OPERATOR(set_source_rgb)
    VISUAL_CONTEXT_OPERATOR(show_text)

    #undef VISUAL_CONTEXT_OPERATOR
  };
} // namespace meevax::visual

#endif // INCLUDED_MEEVAX_VISUAL_CONTEXT_HPP

