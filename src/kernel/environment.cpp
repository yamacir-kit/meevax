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

  auto environment::operator [](external_representation const& name) -> const_reference
  {
    return (*this)[intern(name)];
  }

  auto environment::apply(const_reference f, const_reference xs) -> value_type
  {
    assert(f.is<closure>() or f.is<procedure>() or f.is<continuation>());

    auto dump = std::make_tuple(std::exchange(s, list(f, xs)),
                                std::exchange(e, unit),
                                std::exchange(c, list(make(mnemonic::call),
                                                      make(mnemonic::stop))),
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

  auto resolve_import_set(const_reference import_set) -> value_type
  {
    if (car(import_set).as<symbol>().value == "only")
    {
      let const exported_bindings = resolve_import_set(cadr(import_set));

      let filtered_bindings = unit;

      for (let const& identifier : cddr(import_set))
      {
        if (let const& binding = assq(identifier, exported_bindings); select(binding))
        {
          filtered_bindings = cons(binding, filtered_bindings);
        }
        else
        {
          throw error(make<string>("no such identifier"), identifier);
        }
      }

      return filtered_bindings;
    }
    else if (auto iter = libraries.find(lexical_cast<external_representation>(import_set)); iter != std::end(libraries))
    {
      return std::get<1>(*iter).resolve_export_specs();
    }
    else
    {
      throw error(make<string>("no such library"), import_set);
    }
  }

  auto environment::import_(const_reference import_set) -> void
  {
    let const bindings = resolve_import_set(import_set);

    for (let const& binding : bindings)
    {
      define(binding.as<absolute>().symbol(), binding.as<absolute>().load());
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
      define(variable);

      return car(global());
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
