/*
   Copyright 2018-2022 Tatsuya Yamasaki.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

#ifndef INCLUDED_MEEVAX_VISUAL_CONTEXT_HPP
#define INCLUDED_MEEVAX_VISUAL_CONTEXT_HPP

#include <memory>
#include <utility>

#include <cairo/cairo.h>

namespace meevax::visual
{
  struct context
    : public std::shared_ptr<cairo_t> // TODO UNIQUE_PTR
  {
    template <typename... Ts>
    explicit context(Ts&&... operands)
      : std::shared_ptr<cairo_t> {cairo_create(std::forward<decltype(operands)>(operands)...), cairo_destroy}
    {}

    operator element_type*() const noexcept
    {
      return get();
    }

    #define VISUAL_CONTEXT_OPERATOR(NAME)                                      \
    template <typename... Ts>                                                  \
    decltype(auto) NAME(Ts&&... operands) noexcept                             \
    {                                                                          \
      return cairo_##NAME(*this, std::forward<decltype(operands)>(operands)...); \
    }

    VISUAL_CONTEXT_OPERATOR(arc)
    VISUAL_CONTEXT_OPERATOR(fill)
    VISUAL_CONTEXT_OPERATOR(line_to)
    VISUAL_CONTEXT_OPERATOR(move_to)
    VISUAL_CONTEXT_OPERATOR(new_sub_path)
    VISUAL_CONTEXT_OPERATOR(paint)
    VISUAL_CONTEXT_OPERATOR(rectangle)
    VISUAL_CONTEXT_OPERATOR(select_font_face)
    VISUAL_CONTEXT_OPERATOR(set_font_size)
    VISUAL_CONTEXT_OPERATOR(set_line_width)
    VISUAL_CONTEXT_OPERATOR(set_source_rgb)
    VISUAL_CONTEXT_OPERATOR(set_source_rgba)
    VISUAL_CONTEXT_OPERATOR(show_text)
    VISUAL_CONTEXT_OPERATOR(stroke)
    VISUAL_CONTEXT_OPERATOR(text_extents)

    #undef VISUAL_CONTEXT_OPERATOR
  };
} // namespace meevax::visual

#endif // INCLUDED_MEEVAX_VISUAL_CONTEXT_HPP

