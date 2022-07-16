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
    define(make_symbol(name), value);
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

  auto environment::resolve(const_reference declaration) -> value_type
  {
    if (car(declaration).as<symbol>().value == "only") /* ----------------------
    *
    *  <declaration> = (only <import set> <identifier> ...)
    *
    * ----------------------------------------------------------------------- */
    {
      auto only = [this](let const& import_set)
      {
        return [=](let const& identifiers)
        {
          return filter([&](let const& identity)
                        {
                          return select(memq(identity.as<absolute>().symbol(),
                                             identifiers));
                        },
                        resolve(import_set));
        };
      };

      return only(cadr(declaration))
                 (cddr(declaration));
    }
    else if (car(declaration).as<symbol>().value == "except") /* ---------------
    *
    *  <declaration> = (except <import set> <identifier> ...)
    *
    * ----------------------------------------------------------------------- */
    {
      auto except = [this](let const& import_set)
      {
        return [=](let const& identifiers)
        {
          return filter([&](let const& identity)
                        {
                          return not select(memq(identity.as<absolute>().symbol(),
                                                 identifiers));
                        },
                        resolve(import_set));
        };
      };

      return except(cadr(declaration))
                   (cddr(declaration));
    }
    else if (car(declaration).as<symbol>().value == "prefix") /* ---------------
    *
    *  <declaration> = (prefix <import set> <identifier>)
    *
    * ----------------------------------------------------------------------- */
    {
      auto prefix = [this](let const& import_set)
      {
        return [=](let const& prefixes)
        {
          return map1([&](let const& identity)
                      {
                        return make<absolute>(make_symbol(car(prefixes).as<symbol>().value +
                                                          identity.as<absolute>().symbol().as<symbol>().value),
                                              identity.as<absolute>().load());
                      },
                      resolve(import_set));
        };
      };

      return prefix(cadr(declaration))
                   (cadr(declaration));
    }
    else if (car(declaration).as<symbol>().value == "rename") /* ---------------
    *
    *  <declaration> = (rename <import set>
    *                          (<identifier 1> <identifier 2>) ...)
    *
    * ----------------------------------------------------------------------- */
    {
      auto rename = [this](let const& import_set)
      {
        return [=](let const& renamings)
        {
          return map1([&](let const& identity)
                      {
                        if (let const& renaming = assq(identity.as<absolute>().symbol(),
                                                       renamings);
                            select(renaming))
                        {
                          assert(cadr(renaming).is<symbol>());
                          return make<absolute>(cadr(renaming), identity.as<absolute>().load());
                        }
                        else
                        {
                          return identity;
                        }
                      },
                      resolve(import_set));
        };
      };

      return rename(cadr(declaration))
                   (cddr(declaration));
    }
    else if (auto iter = libraries.find(lexical_cast<external_representation>(declaration)); iter != std::end(libraries))
    {
      return std::get<1>(*iter).resolve();
    }
    else
    {
      throw error(make<string>("No such library"), declaration);
    }
  }

  auto environment::import_(const_reference import_set) -> void
  {
    for (let const& identity : resolve(import_set))
    {
      assert(identity.is<absolute>());

      if (let const& variable = identity.as<absolute>().symbol(); not eq((*this)[variable], undefined) and not interactive)
      {
        throw error(make<string>("In a program or library declaration, it is an error to import the same identifier more than once with different bindings"),
                    list(import_set, variable));
      }
      else
      {
        define(identity.as<absolute>().symbol(), identity.as<absolute>().load());
      }
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
