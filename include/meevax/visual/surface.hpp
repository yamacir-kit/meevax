#ifndef INCLUDED_MEEVAX_VISUAL_SURFACE_HPP
#define INCLUDED_MEEVAX_VISUAL_SURFACE_HPP

#include <cstdint>
#include <memory>

#include <cairo/cairo-xcb.h>

#include <meevax/protocol/accessor.hpp>
#include <meevax/protocol/window.hpp>

namespace meevax::visual
{
  class surface
    : public protocol::window
    , public std::shared_ptr<cairo_surface_t>
  {
    std::uint32_t width, height;

  public:
    explicit surface(const protocol::connection& connection)
      : protocol::window {connection, protocol::root_screen(connection)}
      , std::shared_ptr<cairo_surface_t> {
          cairo_xcb_surface_create(connection, identity, protocol::root_visualtype(connection), 1, 1),
          cairo_surface_destroy
        }
    {}

    explicit surface(const surface& surface)
      : protocol::window {surface.connection, surface.identity}
      , std::shared_ptr<cairo_surface_t> {
          cairo_xcb_surface_create(connection, identity, protocol::root_visualtype(connection), 0, 0),
          cairo_surface_destroy
        }
    {}

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
  };
} // namespace meevax::visual

#endif // INCLUDED_MEEVAX_VISUAL_SURFACE_HPP

