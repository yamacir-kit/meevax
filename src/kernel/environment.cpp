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

namespace meevax
{
inline namespace kernel
{
  auto environment::operator [](const_reference name) -> const_reference
  {
    return rename(name, local()).as<absolute>().binding();
  }

  auto environment::operator [](std::string const& name) -> const_reference
  {
    return (*this)[intern(name)];
  }

  auto environment::define(const_reference name, const_reference value) -> const_reference
  {
    assert(name.is<symbol>());

    return global() = make<absolute>(name, value) | global();
  }

  auto environment::define(std::string const& name, const_reference value) -> const_reference
  {
    return define(intern(name), value);
  }

  auto environment::evaluate(const_reference expression) -> object /* ----------
  *
  *  Since this member function can be called from various contexts, it is
  *  necessary to save the register. In particular, note that the
  *  er-macro-transformer's rename procedure is implemented as an eval with the
  *  macro transformer object as the environment-specifier, so this member
  *  function overrides the VM of the transformer during macro expansion.
  *
  * ------------------------------------------------------------------------- */
  {
    d = cons(s, e, c, d);
    c = compile(context::none, *this, expression, local());
    e = unit;
    s = unit;

    if (is_debug_mode())
    {
      disassemble(debug_port().as<std::ostream>(), c);
    }

    let const result = execute();

    s = pop(d);
    e = pop(d);
    c = pop(d);

    return result;
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

  auto environment::import() -> void
  {
    define<procedure>("free-identifier=?", [](let const& xs, auto&&)
    {
      if (let const& a = car(xs); a.is<symbol>() or a.is_also<identifier>())
      {
        if (let const& b = cadr(xs); b.is<symbol>() or b.is_also<identifier>())
        {
          if (let const& id1 = a.is_also<identifier>() ? a.as<identifier>().symbol() : a)
          {
            if (let const& id2 = b.is_also<identifier>() ? b.as<identifier>().symbol() : b)
            {
              return id1 == id2 ? t : f;
            }
          }
        }
      }

      // if (let const& a = car(xs); a.is<symbol>() or a.is_also<identifier>())
      // {
      //   if (let const& b = cadr(xs); b.is<symbol>() or b.is_also<identifier>())
      //   {
      //     if (auto const& id1 = a.is_also<identifier>() ? a.as<identifier>() : locate(a).as<identifier>(); id1.is_free())
      //     {
      //       if (auto const& id2 = b.is_also<identifier>() ? b.as<identifier>() : locate(b).as<identifier>(); id2.is_free())
      //       {
      //         return id1 == id2 ? t : f;
      //       }
      //     }
      //   }
      // }

      return f;
    });

    define<procedure>("set-batch!",       [this](let const& xs, auto&&) { return batch       = car(xs); });
    define<procedure>("set-debug!",       [this](let const& xs, auto&&) { return debug       = car(xs); });
    define<procedure>("set-interactive!", [this](let const& xs, auto&&) { return interactive = car(xs); });
    define<procedure>("set-prompt!",      [this](let const& xs, auto&&) { return prompt      = car(xs); });
    define<procedure>("set-trace!",       [this](let const& xs, auto&&) { return trace       = car(xs); });
    define<procedure>("set-verbose!",     [this](let const& xs, auto&&) { return verbose     = car(xs); });
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

  auto environment::local() const noexcept -> const_reference
  {
    return first;
  }

  auto environment::local() noexcept -> reference
  {
    return first;
  }

  auto environment::rename(const_reference variable, const_reference frames) const -> object
  {
    if (let const& identifier = notate(variable, frames); select(identifier))
    {
      return identifier;
    }
    else
    {
      return assq(variable, global());
    }
  }

  auto environment::rename(const_reference variable, const_reference frames) -> object
  {
    if (let const& binding = std::as_const(*this).rename(variable, frames); select(binding))
    {
      return binding;
    }
    else
    {
      /* -----------------------------------------------------------------------
       *
       *  At the outermost level of a program, a definition
       *
       *      (define <variable> <expression>)
       *
       *  has essentially the same effect as the assignment expression
       *
       *      (set! <variable> <expression>)
       *
       *  if <variable> is bound to a non-syntax value. However, if <variable>
       *  is not bound, or is a syntactic keyword, then the definition will
       *  bind <variable> to a new location before performing the assignment,
       *  whereas it would be an error to perform a set! on an unbound variable.
       *
       * -------------------------------------------------------------------- */

      let const id = make<absolute>(variable);

      cdr(id) = id; // NOTE: Identifier is self-evaluate if is unbound.

      global() = cons(id, global());

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
