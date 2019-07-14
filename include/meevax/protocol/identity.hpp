#ifndef INCLUDED_MEEVAX_PROTOCOL_IDENTITY_HPP
#define INCLUDED_MEEVAX_PROTOCOL_IDENTITY_HPP

#include <cstdint>
#include <utility>
#include <vector>

#include <xcb/xcb.h>

#include <meevax/protocol/accessor.hpp>
#include <meevax/protocol/connection.hpp>

// REFERENCES
// http://manpages.ubuntu.com/manpages/bionic/man3/xcb_create_window.3.html

namespace meevax::protocol
{
  struct identity
  {
    static inline const protocol::connection connection {};

    using value_type = xcb_window_t;
    const value_type value;

    explicit identity(const value_type& parent = root_screen(connection))
      : value {xcb_generate_id(connection)}
    {
      xcb_create_window(
        connection,
        XCB_COPY_FROM_PARENT, // depth
        value, // child window (this) identity
        parent, // parent window identity
        std::max(0, 0), // x
        std::max(0, 0), // y
        std::max(1, 1), // width (at least 1 required)
        std::max(1, 1), // height (at least 1 required)
        std::max(0, 0), // border width
        XCB_WINDOW_CLASS_INPUT_OUTPUT,
        XCB_COPY_FROM_PARENT, // std::begin(screen {connection})->root_visual,
        0, // window attributes
        nullptr
      );
    }

    ~identity()
    {
      xcb_destroy_window(connection, value);
    }

    decltype(auto) map() const
    {
      return xcb_map_window(connection, value);
    }

    decltype(auto) unmap() const
    {
      return xcb_unmap_window(connection, value);
    }

    template <typename... Ts>
    decltype(auto) configure(const std::uint16_t mask, Ts&&... configurations) const
    {
      const std::vector<std::uint32_t> values {std::forward<Ts>(configurations)...};
      return xcb_configure_window(connection, value, mask, values.data());
    }

    template <typename Mask, typename... Ts>
    decltype(auto) change_attributes(const Mask& mask, Ts&&... attributes) const
    {
      const std::vector<std::uint32_t> values {std::forward<Ts>(attributes)...};
      return xcb_change_window_attributes(connection, value, mask, values.data());
    }
  };
} // namespace meevax::protocol

#endif // INCLUDED_MEEVAX_PROTOCOL_IDENTITY_HPP

