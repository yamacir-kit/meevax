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
  auto environment::define(object const& name, object const& value) -> void
  {
    (*this)[name] = value;
  }

  auto environment::define(std::string const& name, object const& value) -> void
  {
    define(string_to_symbol(name), value);
  }

  auto environment::evaluate(object const& expression) -> object try
  {
    if (car(expression).is<symbol>() and car(expression).as<symbol>().value == "define-library")
    {
      define_library(lexical_cast<std::string>(cadr(expression)), cddr(expression));
      return cadr(expression);
    }
    else if (car(expression).is<symbol>() and car(expression).as<symbol>().value == "import")
    {
      for (let const& form : cdr(expression))
      {
        declare<import_set>(form);
      }

      return unspecified;
    }
    else
    {
      assert(s.is<null>());
      assert(e.is<null>());
      assert(c.is<null>());
      assert(d.is<null>());

      c = optimize(compile(context(), *this, expression, scope()));

      let const result = execute();

      assert(s.is<null>());
      assert(e.is<null>());
      assert(c.is<null>());
      assert(d.is<null>());

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

  auto environment::execute() -> object
  {
    return trace ? machine::execute<true>() : machine::execute();
  }

  auto environment::execute(object const& code) -> object
  {
    c = code;
    return execute();
  }

  auto environment::fork() const -> object
  {
    return make<environment>(*this);
  }

  auto environment::fork(object const& scope) const -> object
  {
    let const copy = make<environment>(*this);
    copy.as<environment>().scope() = scope;
    return copy;
  }

  auto environment::global() const noexcept -> object const&
  {
    return second;
  }

  auto environment::global() noexcept -> object &
  {
    return second;
  }

  auto environment::load(std::string const& s) -> object
  {
    if (let port = make<file_port>(s); port and port.as<file_port>().is_open())
    {
      for (let e = read(port); e != eof_object; e = read(port))
      {
        evaluate(e);
      }

      return unspecified;
    }
    else
    {
      throw file_error(make<string>("failed to open file"),
                       make<string>(s));
    }
  }

  auto environment::scope() const noexcept -> object const&
  {
    return first;
  }

  auto environment::scope() noexcept -> object &
  {
    return first;
  }

  auto environment::identify(object const& variable, object const& scope) const -> object
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

  auto environment::identify(object const& variable, object const& scope) -> object
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
      return car(global() = make<absolute>(variable, undefined) | global());
    }
  }

  auto operator >>(std::istream & is, environment & datum) -> std::istream &
  {
    print("environment::operator >>(std::istream &, environment &)");
    print("read new expression => ", datum.read(is));

    return is;
  }

  auto operator <<(std::ostream & os, environment &) -> std::ostream &
  {
    return write(os, "environment::operator <<(std::ostream &, environment &)\n");
  }

  auto operator <<(std::ostream & os, environment const& datum) -> std::ostream &
  {
    return os << magenta("#,(") << green("environment ") << faint("#;", &datum) << magenta(")");
  }

  template class configurator<environment>;

  template class machine<environment>;

  template class reader<environment>;
} // namespace kernel
} // namespace meevax
