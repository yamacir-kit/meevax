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
    if (car(expression).is<symbol>() and car(expression).as<symbol>() == "define-library")
    {
      define_library(lexical_cast<std::string>(cadr(expression)), cddr(expression));
      return cadr(expression);
    }
    else if (car(expression).is<symbol>() and car(expression).as<symbol>() == "import")
    {
      for (let const& form : cdr(expression))
      {
        declare<import_set>(form);
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

      c = optimize(compile(context(), *this, expression, scope()));

      let const result = execute();

      if (d)
      {
        s = d[0];
        e = d[1];
        c = d[2];
        d = list_tail(d, 3);
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
    else if (let const& identity = machine::identify(variable, scope); is_truthy(identity))
    {
      return identity;
    }
    else
    {
      return assq(variable, global());
    }
  }

  auto environment::identify(object const& variable) const -> object
  {
    return identify(variable, scope());
  }

  auto environment::identify(object const& variable, object const& scope) -> object
  {
    if (not variable.is_also<identifier>())
    {
      return f;
    }
    if (let const& id = std::as_const(*this).identify(variable, scope); is_truthy(id))
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

  auto environment::identify(object const& variable) -> object
  {
    return identify(variable, scope());
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
