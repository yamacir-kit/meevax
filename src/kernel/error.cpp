/*
   Copyright 2018-2025 Tatsuya Yamasaki.

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

#include <meevax/kernel/environment.hpp>
#include <meevax/kernel/error.hpp>
#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/string.hpp>
#include <meevax/kernel/textual_input_port.hpp>

namespace meevax::inline kernel
{
  auto error::irritants() const noexcept -> object const&
  {
    return second;
  }

  auto error::make() const -> object
  {
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

  auto disassemble(std::ostream & output, let const& c, std::size_t depth = 0) -> void
  {
    assert(c.is<pair>());
    assert(car(c).is<instruction>());

    switch (car(c).as<instruction>())
    {
    case instruction::join:
    case instruction::tail_call:
    case instruction::tail_letrec:
    case instruction::return_:
    case instruction::stop:
      output << std::string(depth, ' ') << car(c) << '\n';
      assert(cdr(c).is<null>());
      break;

    case instruction::call:
    case instruction::cons:
    case instruction::drop:
    case instruction::dummy:
    case instruction::letrec:
      output << std::string(depth, ' ') << car(c) << '\n';
      disassemble(output, cdr(c), depth);
      break;

    case instruction::current:
    case instruction::install:
    case instruction::load_absolute:
    case instruction::load_constant:
    case instruction::load_relative:
    case instruction::load_variadic:
    case instruction::store_absolute:
    case instruction::store_relative:
    case instruction::store_variadic:
      output << std::string(depth, ' ') << car(c) << ' ' << cadr(c) << '\n';
      disassemble(output, cddr(c), depth);
      break;

    case instruction::load_closure:
    case instruction::load_continuation:
      output << std::string(depth, ' ') << car(c) << '\n';
      disassemble(output, cadr(c), depth + 2);
      disassemble(output, cddr(c), depth);
      break;

    case instruction::select:
      output << std::string(depth, ' ') << car(c) << '\n';
      disassemble(output, cadr(c), depth + 2);
      disassemble(output, caddr(c), depth + 2);
      disassemble(output, cdddr(c), depth);
      break;

    case instruction::tail_select:
      output << std::string(depth, ' ') << car(c) << '\n';
      disassemble(output, cadr(c), depth + 2);
      disassemble(output, caddr(c), depth + 2);
      break;
    }
  }

  auto error::report(std::ostream & output) const -> std::ostream &
  {
    return output << red("; error! ", what()) << std::endl;
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
} // namespace meevax::kernel
