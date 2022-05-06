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

#include <meevax/kernel/environment.hpp>
#include <meevax/kernel/library.hpp>

namespace meevax
{
inline namespace kernel
{
  auto environment::operator [](const_reference name) -> const_reference
  {
    return identify(name, scope()).as<absolute>().load();
  }

  auto environment::operator [](std::string const& name) -> const_reference
  {
    return (*this)[intern(name)];
  }

  auto environment::apply(const_reference f, const_reference xs) -> object
  {
    auto dump = std::make_tuple(std::exchange(s, list(f, xs)),
                                std::exchange(e, unit),
                                std::exchange(c, list(make<instruction>(mnemonic::call),
                                                      make<instruction>(mnemonic::stop))),
                                std::exchange(d, unit));

    let const result = execute();

    s = std::get<0>(dump);
    e = std::get<1>(dump);
    c = std::get<2>(dump);
    d = std::get<3>(dump);

    return result;
  }

  auto environment::define(const_reference name, const_reference value) -> void
  {
    assert(name.is_also<identifier>());

    global() = make<absolute>(name, value) | global();
  }

  auto environment::define(std::string const& name, const_reference value) -> void
  {
    define(intern(name), value);
  }

  auto environment::evaluate(const_reference expression) -> object
  {
    if (expression.is<pair>() and car(expression).is<symbol>()
                              and car(expression).as<symbol>().value == "import")
    {
      for (let const& import_set : cdr(expression))
      {
        import(import_set);
      }

      return unspecified_object;
    }
    else if (expression.is<pair>() and car(expression).is<symbol>()
                                   and car(expression).as<symbol>().value == "define-library")
    {
      define_library(lexical_cast<std::string>(cadr(expression)), cddr(expression));
      return cadr(expression);
    }
    else
    {
      auto dump = std::make_tuple(std::exchange(s, unit),
                                  std::exchange(e, unit),
                                  std::exchange(c, compile(context::none, *this, expression, scope())),
                                  std::exchange(d, unit));

      if (is_debug_mode())
      {
        disassemble(debug_port().as<std::ostream>(), c);
      }

      let const result = execute();

      s = std::get<0>(dump);
      e = std::get<1>(dump);
      c = std::get<2>(dump);
      d = std::get<3>(dump);

      return result;
    }
  }

  auto environment::execute() -> object
  {
    if (is_trace_mode())
    {
      return machine::execute<option::trace>();
    }
    else
    {
      return machine::execute();
    }
  }

  auto environment::execute(const_reference code) -> object
  {
    c = code;
    return execute();
  }

  auto environment::global() const noexcept -> const_reference
  {
    return second;
  }

  auto environment::global() noexcept -> reference
  {
    return second;
  }

  auto environment::import(const_reference import_set) -> void
  {
    if (car(import_set).as<symbol>().value == "only")
    {
    }
    else if (car(import_set).as<symbol>().value == "except")
    {
    }
    else if (car(import_set).as<symbol>().value == "prefix")
    {
    }
    else if (car(import_set).as<symbol>().value == "rename")
    {
    }
    else // <library name>
    {
      libraries.at(lexical_cast<std::string>(import_set)).export_to(*this);
    }
  }

  auto environment::load(std::string const& s) -> object
  {
    if (let port = make<input_file_port>(s); port and port.as<input_file_port>().is_open())
    {
      for (let e = read(port); e != eof_object; e = read(port))
      {
        evaluate(e);
      }

      return unspecified_object;
    }
    else
    {
      throw file_error(make<string>("failed to open file: " + s));
    }
  }

  auto environment::scope() const noexcept -> const_reference
  {
    return first;
  }

  auto environment::scope() noexcept -> reference
  {
    return first;
  }

  auto environment::identify(const_reference variable, const_reference scope) const -> object
  {
    if (not variable.is_also<identifier>())
    {
      return f;
    }
    else if (let const& identity = machine::identify(variable, scope); select(identity))
    {
      return identity;
    }
    else
    {
      return assq(variable, global());
    }
  }

  auto environment::identify(const_reference variable, const_reference scope) -> object
  {
    if (not variable.is_also<identifier>())
    {
      return f;
    }
    if (let const& id = std::as_const(*this).identify(variable, scope); select(id))
    {
      return id;
    }
    else /* --------------------------------------------------------------------
    *
    *  At the outermost level of a program, a definition
    *
    *      (define <variable> <expression>)
    *
    *  has essentially the same effect as the assignment expression
    *
    *      (set! <variable> <expression>)
    *
    *  if <variable> is bound to a non-syntax value. However, if <variable> is
    *  not bound, or is a syntactic keyword, then the definition will bind
    *  <variable> to a new location before performing the assignment, whereas
    *  it would be an error to perform a set! on an unbound variable.
    *
    * ----------------------------------------------------------------------- */
    {
      define(variable);

      return car(global());
    }
  }

  auto operator >>(std::istream & is, environment & datum) -> std::istream &
  {
    datum.print("environment::operator >>(std::istream &, environment &)");
    datum.print("read new expression => ", datum.read(is));

    return is;
  }

  auto operator <<(std::ostream & os, environment & datum) -> std::ostream &
  {
    return datum.write(os, "environment::operator <<(std::ostream &, environment &)\n");
  }

  auto operator <<(std::ostream & os, environment const& datum) -> std::ostream &
  {
    return os << magenta("#,(") << green("environment ") << faint("#;", &datum) << magenta(")");
  }

  template class configurator<environment>;

  template class machine<environment>;

  template class reader<environment>;

  template class writer<environment>;
} // namespace kernel
} // namespace meevax
