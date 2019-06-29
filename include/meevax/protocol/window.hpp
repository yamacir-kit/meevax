#ifndef INCLUDED_MEEVAX_PROTOCOL_WINDOW_HPP
#define INCLUDED_MEEVAX_PROTOCOL_WINDOW_HPP

#include <cstdint>
#include <vector>

#include <xcb/xcb.h>

#include <meevax/protocol/connection.hpp>

// REFERENCES
// http://manpages.ubuntu.com/manpages/bionic/man3/xcb_create_window.3.html

namespace meevax::protocol
{
  struct window
  {
    const protocol::connection connection;

    const xcb_window_t identity;

    explicit window(const protocol::connection& connection, xcb_window_t parent_identity)
      : connection {connection}
      , identity {xcb_generate_id(connection)}
    {
      xcb_create_window(
        connection,
        XCB_COPY_FROM_PARENT, // depth
        identity, parent_identity,
        std::max(0, 0), // x
        std::max(0, 0), // y
        std::max(1, 0), // width
        std::max(1, 0), // height
        std::max(1, 0), // border width
        XCB_WINDOW_CLASS_INPUT_OUTPUT,
        XCB_COPY_FROM_PARENT, // std::begin(screen {connection})->root_visual,
        0, // window attributes
        nullptr
      );
    }

    ~window()
    {
      xcb_destroy_window(connection, identity);
    }

    decltype(auto) map() const noexcept
    {
      return xcb_map_window(connection, identity);
    }

    decltype(auto) unmap() const noexcept
    {
      return xcb_unmap_window(connection, identity);
    }

    template <typename Mask, typename... Ts>
    decltype(auto) configure(const Mask& mask, Ts&&... configurations)
    {
      const std::vector<std::uint32_t> values {std::forward<Ts>(configurations)...};
      return xcb_configure_window(connection, identity, mask, values.data());
    }

    template <typename Mask, typename... Ts>
    decltype(auto) change_attributes(const Mask& mask, Ts&&... attributes)
    {
      const std::vector<std::uint32_t> values {std::forward<Ts>(attributes)...};
      return xcb_change_window_attributes(connection, identity, mask, values.data());
    }
  };
} // namespace meevax::protocol

#endif // INCLUDED_MEEVAX_PROTOCOL_WINDOW_HPP

