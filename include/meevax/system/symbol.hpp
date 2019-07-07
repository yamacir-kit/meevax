#ifndef INCLUDED_MEEVAX_SYSTEM_SYMBOL_HPP
#define INCLUDED_MEEVAX_SYSTEM_SYMBOL_HPP

#include <iostream>
#include <string>

#include <meevax/visual/behavior.hpp>
#include <meevax/visual/context.hpp>
#include <meevax/visual/surface.hpp>

namespace meevax::system
{
  struct symbol
    : public std::string
  {
    visual::point position;

    template <typename... Ts>
    explicit constexpr symbol(Ts&&... args)
      : std::string {std::forward<Ts>(args)...}
    {}
  };

  auto operator<<(std::ostream& os, const symbol& symbol)
    -> decltype(os)
  {
    if (symbol.empty())
    {
      return os << "\x1b[36m#<symbol " << &static_cast<const std::string&>(symbol) << ">\x1b[0m";
    }
    else
    {
      return os << static_cast<const std::string&>(symbol);
    }
  }

  auto write(const symbol& symbol, const visual::surface& surface)
    -> decltype(surface)
  {
    visual::context context {surface};

    context.set_source_rgb(0x4a / 256.0, 0x69 / 256.0, 0xbd / 256.0);
    context.select_font_face("Latin Modern Roman", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL);
    context.set_font_size(32);

    std::cerr << "[debug] " << symbol << std::endl;

    cairo_text_extents_t extents {};
    context.text_extents(symbol.c_str(), &extents);

    context.move_to(0, extents.height);
    context.show_text(symbol.c_str());

    surface.configure(
      XCB_CONFIG_WINDOW_WIDTH | XCB_CONFIG_WINDOW_HEIGHT,
      static_cast<std::uint32_t>(extents.x_advance),
      static_cast<std::uint32_t>(extents.height)
    );
    surface.size(extents.x_advance, extents.height);

    return surface;
  }

  // auto visualize(visual::surface& surface, symbol& symbol)
  //   -> visual::geometry
  // {
  //   visual::context context {surface};
  //
  //   context.set_source_rgb(0x4a / 256.0, 0x69 / 256.0, 0xbd / 256.0);
  //   context.select_font_face("Latin Modern Roman", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL);
  //   context.set_font_size(32);
  //
  //   cairo_text_extents_t extents {};
  //   context.text_extents(symbol.c_str(), &extents);
  //
  //   context.move_to(
  //     symbol.position[0] - extents.width / 2,
  //     symbol.position[1] + extents.height / 2
  //   );
  //   context.show_text(symbol.c_str());
  //
  //   // extents.x_bearing, extents.y_bearing,
  //   // extents.x_advance, extents.y_advance;
  //
  //   return {&symbol.position};
  // }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_SYMBOL_HPP

