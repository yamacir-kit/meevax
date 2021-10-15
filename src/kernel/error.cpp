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

#include <meevax/kernel/error.hpp>

namespace meevax
{
inline namespace kernel
{
  auto error::irritants() const noexcept -> const_reference
  {
    return cdr(*this);
  }

  auto error::message() const noexcept -> const_reference
  {
    return car(*this);
  }

  auto error::raise() const -> void
  {
    throw *this;
  }

  auto error::what() const -> std::string
  {
    std::stringstream ss {};

    ss << "error: " << static_cast<std::string>(message().as<string>());

    if (irritants())
    {
      ss << ": " << irritants();
    }

    return ss.str();
  }

  auto operator <<(std::ostream & os, error const& datum) -> std::ostream &
  {
    return os << magenta << "#,(" << green << "error " << reset << datum.message() << " " << datum.irritants() << magenta << ")" << reset;
  }

  auto raise(std::string const& message) -> void
  {
    throw error(make<string>(message));
  }

  auto invalid_application(pair::const_reference irritants) -> error
  {
    let static const message = make<string>("invalid application");
    return error(message, irritants);
  }
} // namespace kernel
} // namespace meevax
