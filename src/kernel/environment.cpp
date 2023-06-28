/*
   Copyright 2018-2023 Tatsuya Yamasaki.

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

#include <fstream>

#include <meevax/kernel/environment.hpp>
#include <meevax/kernel/import_set.hpp>
#include <meevax/kernel/input_file_port.hpp>
#include <meevax/kernel/library.hpp>

namespace meevax
{
inline namespace kernel
{
  auto environment::evaluate(object const& expression) -> object try
  {
    auto is = [&](auto name)
    {
      return expression.is<pair>() and expression[0].is<symbol>() and expression[0].as<symbol>() == name;
    };

    if (is("define-library"))
    {
      meevax::define<library>(lexical_cast<std::string>(cadr(expression)), cddr(expression));
      return cadr(expression);
    }
    else if (is("import"))
    {
      for (let const& form : cdr(expression))
      {
        declare<import_set>(form);
      }

      return unspecified;
    }
    else if (is("include"))
    {
      for (let const& command_or_definition : include(cdr(expression), true))
      {
        evaluate(command_or_definition);
      }

      return unspecified;
    }
    else if (is("include-ci"))
    {
      for (let const& command_or_definition : include(cdr(expression), false))
      {
        evaluate(command_or_definition);
      }

      return unspecified;
    }
    else
    {
      /*
         In most cases, the s, e, c, and d registers are all null when evaluate
         is called. However, if environment::evaluate of the same environment
         is called during the execution of environment::evaluate, this is not
         the case, so it is necessary to save the register. For example,
         situations like evaluating
           (eval <expression> (interaction-environment))
         in the REPL.
      */
      if (s or e or c)
      {
        d = cons(std::exchange(s, unit),
                 std::exchange(e, unit),
                 std::exchange(c, unit), d);
      }

      let const result = execute(optimize(compile(expression, bound_variables())));

      if (d)
      {
        s = d[0];
        e = d[1];
        c = d[2];
        d = tail(d, 3);
      }

      return result;
    }
  }
  catch (object const& x)
  {
    if (x.is_also<error>())
    {
      x.as<error>().raise(); // NOTE: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=84476
      return unit;
    }
    else
    {
      throw error(make<string>("uncaught exception"), x);
    }
  }

  auto environment::load(std::string const& s) -> void
  {
    if (auto input = input_file_port(s); input.is_open() and input.get_ready())
    {
      while (not static_cast<std::istream &>(input).eof())
      {
        evaluate(input.read());
      }
    }
    else
    {
      throw file_error(make<string>("failed to open file"),
                       make<string>(s));
    }
  }

  auto environment::operator [](object const& variable) -> object const&
  {
    assert(bound_variables().is<null>());
    assert(e.is<null>());
    assert(identify(variable, unit, unit).is<absolute>());
    return identify(variable, unit, unit).as<absolute>().load();
  }

  auto environment::operator [](std::string const& variable) -> object const&
  {
    return (*this)[make_symbol(variable)];
  }

  auto operator <<(std::ostream & os, environment const& datum) -> std::ostream &
  {
    return os << magenta("#,(") << green("environment ") << faint("#;", &datum) << magenta(")");
  }

  template struct configurator<environment>;

  template struct dynamic_environment<environment>;

  template struct syntactic_environment<environment>;
} // namespace kernel
} // namespace meevax
