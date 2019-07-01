#ifndef INCLUDED_MEEVAX_SYSTEM_SYMBOL_HPP
#define INCLUDED_MEEVAX_SYSTEM_SYMBOL_HPP

#include <iostream>
#include <string>

#include <Eigen/Core>

#include <meevax/behavior/steering.hpp>
#include <meevax/visual/context.hpp>
#include <meevax/visual/surface.hpp>

namespace meevax::system
{
  struct symbol
    : public std::string
  {
    Eigen::Vector2d position;

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

  auto visualize(visual::surface& surface, symbol& symbol)
    -> decltype(surface)
  {
    visual::context context {surface};

    symbol.position += behavior::seek(surface.center - symbol.position);

    context.set_source_rgb(0xC0 / 256.0, 0xC0 / 256.0, 0xC0 / 256.0);
    context.select_font_face("Sans", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL);
    context.set_font_size(32);
    context.move_to(symbol.position[0], symbol.position[1]);
    context.show_text(symbol.c_str());

    return surface;
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_SYMBOL_HPP

