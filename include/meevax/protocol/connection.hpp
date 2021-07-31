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

#ifndef INCLUDED_MEEVAX_PROTOCOL_CONNECTION_HPP
#define INCLUDED_MEEVAX_PROTOCOL_CONNECTION_HPP

#include <memory>
#include <stdexcept>

#include <xcb/xcb.h>

namespace meevax::protocol
{
  struct connection
    : public std::shared_ptr<xcb_connection_t>
  {
    connection()
      : std::shared_ptr<xcb_connection_t> {xcb_connect(nullptr, nullptr), xcb_disconnect}
    {
      if (const auto state {xcb_connection_has_error(*this)}; state)
      {
        switch (state)
        {
        case XCB_CONN_ERROR:
          throw std::runtime_error {"socket errors, pipe errors or other stream errors"};

        case XCB_CONN_CLOSED_EXT_NOTSUPPORTED:
          throw std::runtime_error {"extension not supported"};

        case XCB_CONN_CLOSED_MEM_INSUFFICIENT:
          throw std::runtime_error {"memory not available"};

        case XCB_CONN_CLOSED_REQ_LEN_EXCEED:
          throw std::runtime_error {"exceeding request length that server accepts"};

        case XCB_CONN_CLOSED_PARSE_ERR:
          throw std::runtime_error {"error during parsing display string"};

        case XCB_CONN_CLOSED_INVALID_SCREEN:
          throw std::runtime_error {"the server does not have a screen matching the display"};
        }
      }
    }

    operator element_type*() const noexcept
    {
      return get();
    }

    decltype(auto) flush() const noexcept
    {
      return xcb_flush(*this);
    }
  };
} // namespace meevax::protocol

#endif // INCLUDED_MEEVAX_PROTOCOL_CONNECTION_HPP

