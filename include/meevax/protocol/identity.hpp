#ifndef INCLUDED_MEEVAX_PROTOCOL_IDENTITY_HPP
#define INCLUDED_MEEVAX_PROTOCOL_IDENTITY_HPP

#include <cstdint>
#include <optional>
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
    : public std::optional<xcb_window_t>
  {
    static inline const protocol::connection connection {};

    template <typename... Ts>
    explicit constexpr identity(Ts&&... xs)
      : std::optional<xcb_window_t> {std::forward<Ts>(xs)...}
    {}

    ~identity()
    {
      if (has_value())
      {
        xcb_destroy_window(connection, value());
      }
    }

    void create(const value_type& parent = root_screen(connection))
    {
      emplace(xcb_generate_id(connection));

      xcb_create_window(
        connection,
        XCB_COPY_FROM_PARENT, // depth
        value(), // child window id
        parent, // parent window id
        std::max(0, 0), // x
        std::max(0, 0), // y
        std::max(1, 1), // width
        std::max(1, 1), // height
        std::max(1, 0), // border width
        XCB_WINDOW_CLASS_INPUT_OUTPUT,
        XCB_COPY_FROM_PARENT, // std::begin(screen {connection})->root_visual,
        0, // window attributes
        nullptr
      );
    }

    decltype(auto) map() const
    {
      return xcb_map_window(connection, value());
    }

    decltype(auto) unmap() const noexcept
    {
      return xcb_unmap_window(connection, value());
    }

    template <typename Mask, typename... Ts>
    decltype(auto) configure(const Mask& mask, Ts&&... configurations)
    {
      const std::vector<std::uint32_t> values {std::forward<Ts>(configurations)...};
      return xcb_configure_window(connection, value(), mask, values.data());
    }

    template <typename Mask, typename... Ts>
    decltype(auto) change_attributes(const Mask& mask, Ts&&... attributes)
    {
      const std::vector<std::uint32_t> values {std::forward<Ts>(attributes)...};
      return xcb_change_window_attributes(connection, value(), mask, values.data());
    }
  };
} // namespace meevax::protocol

#endif // INCLUDED_MEEVAX_PROTOCOL_IDENTITY_HPP

