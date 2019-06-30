#ifndef INCLUDED_MEEVAX_VISUAL_WRITE_HPP
#define INCLUDED_MEEVAX_VISUAL_WRITE_HPP

#include <typeinfo>

#include <meevax/system/pair.hpp>
#include <meevax/system/symbol.hpp>
#include <meevax/visual/context.hpp>

namespace meevax::visual
{
  void write(context& context, const system::object& object)
  {
    if (object)
    {
      if (object.type() == typeid(system::symbol))
      {
        context.set_source_rgb(0xC0 / 256.0, 0xC0 / 256.0, 0xC0 / 256.0);
        context.select_font_face("DejaVu Serif", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL);
        context.set_font_size(32);
        context.move_to(100, 100);
        context.show_text(object.as<system::symbol>().c_str());
      }
    }
  }
} // namespace meevax::visual

#endif // INCLUDED_MEEVAX_VISUAL_WRITE_HPP

