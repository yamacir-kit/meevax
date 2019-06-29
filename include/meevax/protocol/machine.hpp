#ifndef INCLUDED_MEEVAX_PROTOCOL_MACHINE_HPP
#define INCLUDED_MEEVAX_PROTOCOL_MACHINE_HPP

#include <utility>

#include <xcb/xcb.h>

#include <meevax/protocol/event.hpp>
#include <meevax/protocol/window.hpp>

namespace meevax::protocol
{
  struct machine
    : public window
  {
    template <typename... Ts>
    explicit machine(Ts&&... xs)
      : window {std::forward<Ts>(xs)...}
    {}

    void execute()
    {
      for (event event {nullptr}; event.wait(connection); connection.flush())
      {
        std::cerr << "; event " << event.type() << "\t; " << event->sequence << "; ";

        switch (event.type())
        {
        }
      }
    }
  };
} // namespace meevax::protocol

#endif // INCLUDED_MEEVAX_PROTOCOL_MACHINE_HPP

