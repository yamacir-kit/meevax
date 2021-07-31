/*
   Copyright 2018-2021 Tatsuya Yamasaki.

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

#ifndef INCLUDED_MEEVAX_VISUAL_SURFACE_HPP
#define INCLUDED_MEEVAX_VISUAL_SURFACE_HPP

#include <cstdint>
#include <functional>
#include <memory>

#include <cairo/cairo-xcb.h>

#include <meevax/protocol/accessor.hpp>
#include <meevax/protocol/machine.hpp>
#include <meevax/visual/context.hpp>

namespace meevax::visual
{
  constexpr std::uint32_t events
  {
      XCB_EVENT_MASK_NO_EVENT
    // | XCB_EVENT_MASK_KEY_PRESS
    // | XCB_EVENT_MASK_KEY_RELEASE
    | XCB_EVENT_MASK_BUTTON_PRESS
    | XCB_EVENT_MASK_BUTTON_RELEASE
    // | XCB_EVENT_MASK_ENTER_WINDOW
    // | XCB_EVENT_MASK_LEAVE_WINDOW
    // | XCB_EVENT_MASK_POINTER_MOTION
    // | XCB_EVENT_MASK_POINTER_MOTION_HINT
    // | XCB_EVENT_MASK_BUTTON_1_MOTION
    // | XCB_EVENT_MASK_BUTTON_2_MOTION
    // | XCB_EVENT_MASK_BUTTON_3_MOTION
    // | XCB_EVENT_MASK_BUTTON_4_MOTION
    // | XCB_EVENT_MASK_BUTTON_5_MOTION
    | XCB_EVENT_MASK_BUTTON_MOTION
    // | XCB_EVENT_MASK_KEYMAP_STATE
    | XCB_EVENT_MASK_EXPOSURE
    // | XCB_EVENT_MASK_VISIBILITY_CHANGE
    | XCB_EVENT_MASK_STRUCTURE_NOTIFY
    // | XCB_EVENT_MASK_RESIZE_REDIRECT
    // | XCB_EVENT_MASK_SUBSTRUCTURE_NOTIFY
    // | XCB_EVENT_MASK_SUBSTRUCTURE_REDIRECT
    // | XCB_EVENT_MASK_FOCUS_CHANGE
    // | XCB_EVENT_MASK_PROPERTY_CHANGE
    // | XCB_EVENT_MASK_COLOR_MAP_CHANGE
    // | XCB_EVENT_MASK_OWNER_GRAB_BUTTON
  };

  struct surface
    : public protocol::machine<surface, events>
    , public std::shared_ptr<cairo_surface_t>
  {
    explicit surface()
      : protocol::machine<surface, events> {}
      , std::shared_ptr<cairo_surface_t> {
          cairo_xcb_surface_create(connection, value, protocol::root_visualtype(connection), 1, 1),
          cairo_surface_destroy
        }
    {
      map();
    }

    explicit surface(const surface& parent)
      : protocol::machine<surface, events> {parent.value}
      , std::shared_ptr<cairo_surface_t> {
          cairo_xcb_surface_create(connection, value, protocol::root_visualtype(connection), 1, 1),
          cairo_surface_destroy
        }
    {
      map();
    }

    operator element_type*() const noexcept
    {
      return get();
    }

    decltype(auto) flush()
    {
      cairo_surface_flush(*this);
      connection.flush();
    }

    void size(std::uint32_t width, std::uint32_t height) const noexcept
    {
      cairo_xcb_surface_set_size(*this, width, height);
    }

    // void operator()(const std::unique_ptr<xcb_expose_event_t>)
    // {}

    void operator()(const std::unique_ptr<xcb_configure_notify_event_t> event)
    {
      size(event->width, event->height);
    }

    int offset_x, offset_y;

    void operator()(const std::unique_ptr<xcb_button_press_event_t> event)
    {
      if (event->event == value && not pressed)
      {
        offset_x = event->event_x;
        offset_y = event->event_y;
      }
    }

    void operator()(const std::unique_ptr<xcb_motion_notify_event_t> event)
    {
      if (event->event == value && pressed)
      {
        configure(
          XCB_CONFIG_WINDOW_X | XCB_CONFIG_WINDOW_Y,
          static_cast<std::uint32_t>(static_cast<int>(event->event_x) - offset_x),
          static_cast<std::uint32_t>(static_cast<int>(event->event_y) - offset_y)
        );
      }
    }
  };
} // namespace meevax::visual

#endif // INCLUDED_MEEVAX_VISUAL_SURFACE_HPP

