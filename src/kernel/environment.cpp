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
  auto environment::define(const_reference name, const_reference value) -> void
  {
    (*this)[name] = value;
  }

  auto environment::define(external_representation const& name, const_reference value) -> void
  {
    define(intern(name), value);
  }

  auto environment::evaluate(const_reference expression) -> value_type
  {
    if (expression.is<pair>() and car(expression).is<symbol>()
                              and car(expression).as<symbol>().value == "define-library")
    {
      define_library(lexical_cast<external_representation>(cadr(expression)), cddr(expression));
      return cadr(expression);
    }
    else if (expression.is<pair>() and car(expression).is<symbol>()
                                   and car(expression).as<symbol>().value == "import")
    {
      for (let const& import_set : cdr(expression))
      {
        import_(import_set);
      }

      return unspecified;
    }
    else
    {
      assert(s.is<null>());
      assert(e.is<null>());
      assert(c.is<null>());
      assert(d.is<null>());

      c = compile(context(), *this, expression, scope());

      c = optimize(c);

      let const result = execute();

      assert(s.is<null>());
      assert(e.is<null>());
      assert(c.is<null>());
      assert(d.is<null>());

      return result;
    }
  }

  auto environment::execute() -> value_type
  {
    return trace ? machine::execute<true>() : machine::execute();
  }

  auto environment::execute(const_reference code) -> value_type
  {
    c = code;
    return execute();
  }

  auto environment::fork() const -> value_type
  {
    return make<environment>(*this);
  }

  auto environment::fork(const_reference scope) const -> value_type
  {
    let const copy = make<environment>(*this);
    copy.as<environment>().scope() = scope;
    return copy;
  }

  auto environment::global() const noexcept -> const_reference
  {
    return second;
  }

  auto environment::global() noexcept -> reference
  {
    return second;
  }

  auto resolve(const_reference import_set) -> value_type
  {
    if (car(import_set).as<symbol>().value == "only") /* -----------------------
    *
    *  (only <import set> <identifier> ...)
    *
    * ----------------------------------------------------------------------- */
    {
      let const identities = resolve(cadr(import_set));

      return map1([&](let const& identifier)
      {
        if (let const& identity = assq(identifier, identities); select(identity))
        {
          return identity;
        }
        else
        {
          throw error(make<string>("No such identifier"), identity);
        }
      }, cddr(import_set));
    }
    else if (car(import_set).as<symbol>().value == "except") /* ----------------
    *
    *  (except <import set> <identifier> ...)
    *
    * ----------------------------------------------------------------------- */
    {
      let result = unit;

      for (let const& id : resolve(cadr(import_set)))
      {
        if (let const& x = memq(id, cddr(import_set)); not select(x))
        {
          result = cons(id, result);
        }
      }

      return result;
    }
    else if (car(import_set).as<symbol>().value == "prefix") /* ----------------
    *
    *  (prefix <import set> <identifier> ...)
    *
    * ----------------------------------------------------------------------- */
    {
      throw error(make<string>("Unsupported"), car(import_set));
    }
    else if (car(import_set).as<symbol>().value == "rename") /* ----------------
    *
    *  (rename <import set>
    *          (<identifier 1> <identifier 2>) ...)
    *
    * ----------------------------------------------------------------------- */
    {
      throw error(make<string>("Unsupported"), car(import_set));
    }
    else if (auto iter = libraries.find(lexical_cast<external_representation>(import_set)); iter != std::end(libraries))
    {
      return std::get<1>(*iter).exported_identities();
    }
    else
    {
      throw error(make<string>("No such library"), import_set);
    }
  }

  auto environment::import_(const_reference import_set) -> void
  {
    for (let const& identity : resolve(import_set))
    {
      assert(identity.is<absolute>());
      define(identity.as<absolute>().symbol(), identity.as<absolute>().load());
    }
  }

  auto environment::import_(external_representation const& import_set) -> void
  {
    import_(read(import_set));
  }

  auto environment::load(external_representation const& s) -> value_type
  {
    if (let port = make<input_file_port>(s); port and port.as<input_file_port>().is_open())
    {
      for (let e = read(port); e != eof_object; e = read(port))
      {
        evaluate(e);
      }

      return unspecified;
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

  auto environment::identify(const_reference variable, const_reference scope) const -> value_type
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

  auto environment::identify(const_reference variable, const_reference scope) -> value_type
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
