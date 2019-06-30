#ifndef INCLUDED_MEEVAX_PROTOCOL_MACHINE_HPP
#define INCLUDED_MEEVAX_PROTOCOL_MACHINE_HPP

#include <type_traits>
#include <utility>

#include <xcb/xcb.h>

#include <meevax/protocol/event.hpp>
#include <meevax/protocol/window.hpp>

namespace meevax::protocol
{
  template <typename Visual, auto Event>
  struct machine
    : public window
  {
    template <typename... Ts>
    explicit machine(Ts&&... xs)
      : window {std::forward<Ts>(xs)...}
    {
      change_attributes(XCB_CW_EVENT_MASK, Event);
    }

    #define TRANSFER_THE_EVENT_IF_VISUALIZABLE(EVENT_NAME)                     \
    if constexpr (std::is_invocable<                                           \
                    Visual, std::unique_ptr<xcb_##EVENT_NAME##_event_t>        \
                  >::value)                                                    \
    {                                                                          \
      std::cerr << #EVENT_NAME << std::endl;                                   \
      static_cast<Visual&>(*this)(                                             \
        event.release_as<xcb_##EVENT_NAME##_event_t>()                         \
      );                                                                       \
    }                                                                          \
    else                                                                       \
    {                                                                          \
      std::cerr << #EVENT_NAME << " unimplemented" << std::endl;               \
    }                                                                          \
    break;

    void visualize()
    {
      for (event event {nullptr}; event.wait(connection); connection.flush())
      {
        std::cerr << "; event " << event.type() << "\t; " << event->sequence << "; ";

        switch (event.type())
        {
        case XCB_KEY_PRESS:                                                //  2
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(key_press)

        case XCB_KEY_RELEASE:                                              //  3
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(key_release)

        case XCB_BUTTON_PRESS:                                             //  4
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(button_press)

        case XCB_BUTTON_RELEASE:                                           //  5
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(button_release)

        case XCB_MOTION_NOTIFY:                                            //  6
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(motion_notify)

        case XCB_ENTER_NOTIFY:                                             //  7
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(enter_notify)

        case XCB_LEAVE_NOTIFY:                                             //  8
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(leave_notify)

        case XCB_FOCUS_IN:                                                 //  9
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(focus_in)

        case XCB_FOCUS_OUT:                                                // 10
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(focus_out)

        case XCB_KEYMAP_NOTIFY:                                            // 11
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(keymap_notify)

        case XCB_EXPOSE:                                                   // 12
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(expose)

        case XCB_GRAPHICS_EXPOSURE:                                        // 13
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(graphics_exposure)

        case XCB_NO_EXPOSURE:                                              // 14
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(no_exposure)

        case XCB_VISIBILITY_NOTIFY:                                        // 15
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(visibility_notify)

        case XCB_CREATE_NOTIFY:                                            // 16
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(create_notify)

        case XCB_DESTROY_NOTIFY:                                           // 17
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(destroy_notify)

        case XCB_UNMAP_NOTIFY:                                             // 18
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(unmap_notify)

        case XCB_MAP_NOTIFY:                                               // 19
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(map_notify)

        case XCB_MAP_REQUEST:                                              // 20
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(map_request)

        case XCB_REPARENT_NOTIFY:                                          // 21
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(reparent_notify)

        case XCB_CONFIGURE_NOTIFY:                                         // 22
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(configure_notify)

        case XCB_CONFIGURE_REQUEST:                                        // 23
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(configure_request)

        case XCB_GRAVITY_NOTIFY:                                           // 24
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(gravity_notify)

        case XCB_RESIZE_REQUEST:                                           // 25
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(resize_request)

        case XCB_CIRCULATE_NOTIFY:                                         // 26
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(circulate_notify)

        case XCB_CIRCULATE_REQUEST:                                        // 27
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(circulate_request)

        case XCB_PROPERTY_NOTIFY:                                          // 28
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(property_notify)

        case XCB_SELECTION_CLEAR:                                          // 29
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(selection_clear)

        case XCB_SELECTION_REQUEST:                                        // 30
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(selection_request)

        case XCB_SELECTION_NOTIFY:                                         // 31
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(selection_notify)

        case XCB_COLORMAP_NOTIFY:                                          // 32
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(colormap_notify)

        case XCB_CLIENT_MESSAGE:                                           // 33
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(client_message)

        case XCB_MAPPING_NOTIFY:                                           // 34
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(mapping_notify)

        case XCB_GE_GENERIC:                                               // 35
          TRANSFER_THE_EVENT_IF_VISUALIZABLE(ge_generic)
        }
      }
    }
  };
} // namespace meevax::protocol

#endif // INCLUDED_MEEVAX_PROTOCOL_MACHINE_HPP

