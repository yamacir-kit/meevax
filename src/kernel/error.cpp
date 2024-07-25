/*
   Copyright 2018-2024 Tatsuya Yamasaki.

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
#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/string.hpp>
#include <meevax/kernel/textual_input_port.hpp>

namespace meevax
{
inline namespace kernel
{
  auto error::irritants() const noexcept -> object const&
  {
    return second;
  }

  auto error::make() const -> object
  {
    /*
       When a class that publicly inherits pair is made, the pair constructor
       is called with priority, so not all data members are copied.
    */
    return meevax::make(*this);
  }

  auto error::message() const noexcept -> object const&
  {
    return first;
  }

  auto error::raise() const -> void
  {
    throw *this;
  }

  template <typename T>
  struct reverse
  {
    T & x;

    explicit reverse(T & x)
      : x { x }
    {}

    auto begin()
    {
      return std::rbegin(x);
    }

    auto end()
    {
      return std::rend(x);
    }
  };

  auto error::report(std::ostream & output) const -> std::ostream &
  {
    output << red("; error! ", what()) << "\n";

    for (auto const& [doing, x] : reverse(contexts))
    {
      switch (doing)
      {
      case in::evaluating: // x is expression
        output << ";   ";

        if (auto read_context = textual_input_port::contexts.find(x.get()); read_context != textual_input_port::contexts.end())
        {
          output << read_context->second;
        }
        else
        {
          output << "{annonymous-input-source}";
        }

        output << ": " << x << std::endl;
        break;

      case in::running:
        break;

      default:
        assert(false);
        break;
      }
    }

    return output;
  }

  auto error::what() const noexcept -> char const*
  {
    try
    {
      if (cache.empty())
      {
        auto output = std::stringstream();

        output << static_cast<std::string>(message().as<string>());

        if (irritants())
        {
          output << ": " << irritants();
        }

        cache = output.str();
      }

      return cache.c_str();
    }
    catch (...)
    {
      std::cerr << "meevax::error::what failed to create an explanatory string for std::exception::what" << std::endl;
      std::exit(EXIT_FAILURE);
    }
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
} // namespace kernel
} // namespace meevax
