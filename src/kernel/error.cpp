/*
   Copyright 2018-2022 Tatsuya Yamasaki.

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
    return second;
  }

  auto error::message() const noexcept -> const_reference
  {
    return first;
  }

  auto error::raise() const -> void
  {
    throw *this;
  }

  auto error::what() const -> external_representation
  {
    std::stringstream ss {};

    ss << "error: " << static_cast<external_representation>(message().as<string>());

    if (irritants())
    {
      ss << ": " << irritants();
    }

    return ss.str();
  }

  auto operator <<(std::ostream & os, error const& datum) -> std::ostream &
  {
    os << magenta("#,(") << green("error ") << datum.message();

    if (not datum.irritants().is<null>())
    {
      os << " " << datum.irritants();
    }

    return os << magenta(")");
  }

  auto raise(external_representation const& message) -> void
  {
    throw error(make<string>(message));
  }

  auto invalid_application(const_reference irritants) -> error
  {
    let static const message = make<string>("invalid application");
    return error(message, irritants);
  }
} // namespace kernel
} // namespace meevax
