#ifndef INCLUDED_MEEVAX_PROTOCOL_EVENT_HPP
#define INCLUDED_MEEVAX_PROTOCOL_EVENT_HPP

#include <memory>
#include <utility>

#include <xcb/xcb.h>

namespace meevax::protocol
{
  struct event
    : public std::unique_ptr<xcb_generic_event_t>
  {
    template <typename... Ts>
    explicit event(Ts&&... xs)
      : std::unique_ptr<xcb_generic_event_t> {std::forward<Ts>(xs)...}
    {}

    auto type() const noexcept
    {
      return get()->response_type & ~0x80;
    }
  };
} // namespace meevax::protocol

#endif // INCLUDED_MEEVAX_PROTOCOL_EVENT_HPP

