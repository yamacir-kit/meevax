#ifndef INCLUDED_MEEVAX_VISUAL_SURFACE_HPP
#define INCLUDED_MEEVAX_VISUAL_SURFACE_HPP

#include <cstdint>
#include <memory>

#include <cairo/cairo-xcb.h>

#include <meevax/protocol/accessor.hpp>
#include <meevax/protocol/machine.hpp>
#include <meevax/visual/context.hpp>

namespace meevax::visual
{
  class surface
    : public protocol::machine<surface>
    , public std::shared_ptr<cairo_surface_t>
  {
    std::uint32_t width, height;

    using machine = protocol::machine<surface>;

  public:
    explicit surface(const protocol::connection& connection)
      : machine {connection, protocol::root_screen(connection)}
      , std::shared_ptr<cairo_surface_t> {
          cairo_xcb_surface_create(connection, identity, protocol::root_visualtype(connection), 1, 1),
          cairo_surface_destroy
        }
    {}

    // explicit surface(const surface& surface)
    //   : machine {surface.connection, surface.identity}
    //   , std::shared_ptr<cairo_surface_t> {
    //       cairo_xcb_surface_create(connection, identity, protocol::root_visualtype(connection), 0, 0),
    //       cairo_surface_destroy
    //     }
    // {}

    operator element_type*() const noexcept
    {
      return get();
    }

    void size(std::uint32_t width, std::uint32_t height) noexcept
    {
      width = width;
      height = height;
      cairo_xcb_surface_set_size(*this, width, height);
      cairo_surface_flush(*this);
    }

  public:
    void operator()(const std::unique_ptr<xcb_expose_event_t>)
    {
      context context {*this};
      context.set_source_rgb(0xF5 / 256.0, 0xF5 / 256.0, 0xF5 / 256.0);
      context.paint();
    }
  };
} // namespace meevax::visual

#endif // INCLUDED_MEEVAX_VISUAL_SURFACE_HPP

