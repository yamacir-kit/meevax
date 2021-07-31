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

#ifndef INCLUDED_MEEVAX_PROTOCOL_EVENT_HPP
#define INCLUDED_MEEVAX_PROTOCOL_EVENT_HPP

#include <memory>
#include <utility>

#include <meevax/protocol/connection.hpp>

namespace meevax::protocol
{
  struct event
    : public std::unique_ptr<xcb_generic_event_t>
  {
    template <typename... Ts>
    explicit event(Ts&&... operands)
      : std::unique_ptr<xcb_generic_event_t> {std::forward<decltype(operands)>(operands)...}
    {}

    auto type() const noexcept
    {
      return get()->response_type & ~0x80;
    }

    decltype(auto) wait(const connection& connection)
    {
      reset(xcb_wait_for_event(connection));
      return *this;
    }

    template <typename T>
    auto release_as()
    {
      return std::unique_ptr<T> {reinterpret_cast<T*>((*this).release())};
    }
  };
} // namespace meevax::protocol

#endif // INCLUDED_MEEVAX_PROTOCOL_EVENT_HPP

