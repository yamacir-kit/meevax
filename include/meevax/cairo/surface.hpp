#ifndef INCLUDED_MEEVAX_CAIRO_SURFACE_HPP
#define INCLUDED_MEEVAX_CAIRO_SURFACE_HPP

#include <cstdint>
#include <memory>

#include <cairo/cairo-xcb.h>

#include <meevax/xcb/accessor.hpp>
#include <meevax/xcb/window.hpp>

namespace meevax::cairo
{
  struct surface
    : public xcb::window
    , public std::shared_ptr<cairo_surface_t>
  {
    explicit surface(const xcb::connection& connection)
      : xcb::window {connection, xcb::root_screen(connection)}
      , std::shared_ptr<cairo_surface_t> {
          cairo_xcb_surface_create(connection, identity, xcb::root_visualtype(connection), 1, 1),
          cairo_surface_destroy
        }
    {}

    explicit surface(const surface& surface)
      : xcb::window {surface.connection, surface.identity}
      , std::shared_ptr<cairo_surface_t> {
          cairo_xcb_surface_create(connection, identity, xcb::root_visualtype(connection), 0, 0),
          cairo_surface_destroy
        }
    {}

    operator element_type*() const noexcept
    {
      return get();
    }

    void size(std::uint32_t width, std::uint32_t height) noexcept
    {
      cairo_xcb_surface_set_size(*this, width, height);
      cairo_surface_flush(*this);
    }
  };
} // namespace meevax::cairo

#endif // INCLUDED_MEEVAX_CAIRO_SURFACE_HPP

