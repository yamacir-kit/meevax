/*
   Copyright 2018-2021 Tatsuya Yamasaki.

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
#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace kernel
{
  auto environment::operator [](const_reference name) -> const_reference
  {
    return cdr(rename(name));
  }

  auto environment::operator [](std::string const& name) -> const_reference
  {
    return (*this)[intern(name)];
  }

  auto environment::build(continuation const& k) -> void
  {
    auto current_compiler = [this](auto&&, auto&&, auto&& expression, auto&& frames, auto&&)
    {
      return compile(context::outermost, *this, expression, frames);
    };

    s = k.s();
    e = k.e();
    c = k.c().as<syntactic_continuation>().apply(current_compiler);
    d = k.d();

    form() = execute();

    // assert(form().is<closure>());
  }

  auto environment::current_expression() const -> const_reference
  {
    return car(form());
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

  auto environment::dynamic_environment() const -> const_reference
  {
    return cdr(form());
  }

  auto environment::evaluate(const_reference expression) -> object
  {
    c = compile(context::none, *this, expression);

    if (is_debug_mode())
    {
      disassemble(debug_port().as<std::ostream>(), c);
    }

    return execute();
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

  auto environment::form() const noexcept -> const_reference
  {
    return car(*this);
  }

  auto environment::form() noexcept -> reference
  {
    return const_cast<reference>(std::as_const(*this).form());
  }

  auto environment::global() const noexcept -> const_reference
  {
    return cdr(*this);
  }

  auto environment::global() noexcept -> reference
  {
    return const_cast<reference>(std::as_const(*this).global());
  }

  auto environment::import() -> void
  {
    define<procedure>("free-identifier=?", [](let const& xs)
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

    define<procedure>("set-batch!",       [this](let const& xs) { return batch       = car(xs); });
    define<procedure>("set-debug!",       [this](let const& xs) { return debug       = car(xs); });
    define<procedure>("set-interactive!", [this](let const& xs) { return interactive = car(xs); });
    define<procedure>("set-prompt!",      [this](let const& xs) { return prompt      = car(xs); });
    define<procedure>("set-trace!",       [this](let const& xs) { return trace       = car(xs); });
    define<procedure>("set-verbose!",     [this](let const& xs) { return verbose     = car(xs); });
  }

  auto environment::load(std::string const& s) -> object
  {
    write(debug_port(), header(__func__), "open ", s, " => ");

    if (let port = make<input_file_port>(s); port and port.as<input_file_port>().is_open())
    {
      write(debug_port(), t, "\n");

      for (let e = read(port); e != eof_object; e = read(port))
      {
        write(debug_port(), header(__func__), e, "\n");

        evaluate(e);
      }

      return unspecified_object;
    }
    else
    {
      write(debug_port(), f, "\n");

      throw file_error(make<string>("failed to open file: " + s));
    }
  }

  auto environment::load(const_reference x) -> object
  {
    if (x.is<symbol>())
    {
      return load(x.as<symbol>());
    }
    else if (x.is<string>())
    {
      return load(x.as<string>());
    }
    else
    {
      throw file_error(make<string>(cat, __FILE__, ":", __LINE__, ":", __func__));
    }
  }

  auto environment::macroexpand(const_reference keyword, const_reference form) -> object
  {
    push(d, s, e, cons(make<instruction>(mnemonic::stop), c)); // XXX ???

    s = unit;
    e = cons(cons(keyword, cdr(form)), dynamic_environment());
    c = current_expression();

    return execute();
  }

  auto environment::rename(const_reference variable) -> const_reference
  {
    if (let const& binding = assq(variable, global()); if_(binding))
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

  auto environment::rename(const_reference variable, const_reference frames) -> object
  {
    if (let const& identifier = notate(variable, frames); if_(identifier))
    {
      return identifier;
    }
    else
    {
      return rename(variable);
    }
  }

  auto environment::rename(const_reference variable) const -> const_reference
  {
    return assq(variable, global());
  }

  auto environment::rename(const_reference variable, const_reference frames) const -> object
  {
    if (let const& identifier = notate(variable, frames); if_(identifier))
    {
      return identifier;
    }
    else
    {
      return rename(variable); // NOTE: In the const version, rename does not extend the global-environment.
    }
  }

  auto operator >>(std::istream & is, environment & datum) -> std::istream &
  {
    datum.print("environment::operator >>(std::istream &, environment &)");
    datum.print("read new expression => ", datum.read(is));

    // sk.print("program == ", sk.program(), "current_expression is ", sk.current_expression());

    return is;
  }

  auto operator <<(std::ostream & os, environment & datum) -> std::ostream &
  {
    // TODO
    // Evaluate current_expression, and write the evaluation to ostream.

    return datum.write(os, "environment::operator <<(std::ostream &, environment &)\n");
  }

  auto operator <<(std::ostream & os, environment const& datum) -> std::ostream &
  {
    return os << magenta << "#,("
              << green << "environment" << reset
              << faint << " #;" << &datum << reset
              << magenta << ")" << reset;
  }

  template class configurator<environment>;

  template class machine<environment>;

  template class reader<environment>;

  template class writer<environment>;
} // namespace kernel
} // namespace meevax
