#ifndef INCLUDED_MEEVAX_PROTOCOL_MACHINE_HPP
#define INCLUDED_MEEVAX_PROTOCOL_MACHINE_HPP

#include <utility>

#include <xcb/xcb.h>

#include <meevax/protocol/event.hpp>
#include <meevax/protocol/window.hpp>

namespace meevax::protocol
{
  template <typename Visual>
  struct machine
    : public window
  {
    template <typename... Ts>
    explicit machine(Ts&&... xs)
      : window {std::forward<Ts>(xs)...}
    {
      change_attributes(XCB_CW_EVENT_MASK,
          XCB_EVENT_MASK_NO_EVENT
        // | XCB_EVENT_MASK_KEY_PRESS
        // | XCB_EVENT_MASK_KEY_RELEASE
        // | XCB_EVENT_MASK_BUTTON_PRESS
        // | XCB_EVENT_MASK_BUTTON_RELEASE
        // | XCB_EVENT_MASK_ENTER_WINDOW
        // | XCB_EVENT_MASK_LEAVE_WINDOW
        // | XCB_EVENT_MASK_POINTER_MOTION
        // | XCB_EVENT_MASK_POINTER_MOTION_HINT
        // | XCB_EVENT_MASK_BUTTON_1_MOTION
        // | XCB_EVENT_MASK_BUTTON_2_MOTION
        // | XCB_EVENT_MASK_BUTTON_3_MOTION
        // | XCB_EVENT_MASK_BUTTON_4_MOTION
        // | XCB_EVENT_MASK_BUTTON_5_MOTION
        // | XCB_EVENT_MASK_BUTTON_MOTION
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
      );
    }

    void visualize()
    {
      for (event event {nullptr}; event.wait(connection); connection.flush())
      {
        std::cerr << "; event " << event.type() << "\t; " << event->sequence << "; ";

        switch (event.type())
        {
        case XCB_KEY_PRESS:                                                //  2
          std::cerr << "key-press" << std::endl;
          break;

        case XCB_KEY_RELEASE:                                              //  3
        case XCB_BUTTON_PRESS:                                             //  4
        case XCB_BUTTON_RELEASE:                                           //  5
        case XCB_MOTION_NOTIFY:                                            //  6
        case XCB_ENTER_NOTIFY:                                             //  7
        case XCB_LEAVE_NOTIFY:                                             //  8
        case XCB_FOCUS_IN:                                                 //  9
        case XCB_FOCUS_OUT:                                                // 10
        case XCB_KEYMAP_NOTIFY:                                            // 11
          std::cerr << "unimplemented" << std::endl;
          break;

        case XCB_EXPOSE:                                                   // 12
          std::cerr << "expose" << std::endl;
          static_cast<Visual&>(*this)(event.release_as<xcb_expose_event_t>());
          break;

        case XCB_GRAPHICS_EXPOSURE:                                        // 13
        case XCB_NO_EXPOSURE:                                              // 14
        case XCB_VISIBILITY_NOTIFY:                                        // 15
        case XCB_CREATE_NOTIFY:                                            // 16
        case XCB_DESTROY_NOTIFY:                                           // 17
        case XCB_UNMAP_NOTIFY:                                             // 18
        case XCB_MAP_NOTIFY:                                               // 19
        case XCB_MAP_REQUEST:                                              // 20
        case XCB_REPARENT_NOTIFY:                                          // 21
          std::cerr << "unimplemented" << std::endl;
          break;

        case XCB_CONFIGURE_NOTIFY:                                         // 22
          std::cerr << "configure-notify" << std::endl;
          static_cast<Visual&>(*this)(event.release_as<xcb_configure_notify_event_t>());
          break;

        case XCB_CONFIGURE_REQUEST:                                        // 23
          std::cerr << "configure-request" << std::endl;
          break;

        case XCB_GRAVITY_NOTIFY:                                           // 24
        case XCB_RESIZE_REQUEST:                                           // 25
        case XCB_CIRCULATE_NOTIFY:                                         // 26
        case XCB_CIRCULATE_REQUEST:                                        // 27
        case XCB_PROPERTY_NOTIFY:                                          // 28
        case XCB_SELECTION_CLEAR:                                          // 29
        case XCB_SELECTION_REQUEST:                                        // 30
        case XCB_SELECTION_NOTIFY:                                         // 31
        case XCB_COLORMAP_NOTIFY:                                          // 32
        case XCB_CLIENT_MESSAGE:                                           // 33
        case XCB_MAPPING_NOTIFY:                                           // 34
        case XCB_GE_GENERIC:                                               // 35
          std::cerr << "unimplemented" << std::endl;
          break;
        }
      }
    }
  };
} // namespace meevax::protocol

#endif // INCLUDED_MEEVAX_PROTOCOL_MACHINE_HPP

