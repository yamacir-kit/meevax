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

#include <meevax/kernel/syntactic_continuation.hpp>
#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace kernel
{
  auto syntactic_continuation::operator [](const_reference name) -> const_reference
  {
    return cdr(machine::locate(name));
  }

  auto syntactic_continuation::operator [](std::string const& name) -> const_reference
  {
    return (*this)[intern(name)];
  }

  auto syntactic_continuation::build() -> void
  {
    /* ---- NOTE -------------------------------------------------------------
     *
     *  If this class was instantiated by the FORK instruction, the instance
     *  will have received the compilation continuation as a constructor
     *  argument.
     *
     *  The car part contains the registers of the virtual Lisp machine
     *  (s e c . d). The cdr part is set to the global environment at the
     *  time the FORK instruction was executed.
     *
     *  Here, the value in the c register is the operand of the FORK
     *  instruction. The operand of the FORK instruction is a pair of a
     *  lambda expression form passed to the syntax fork/csc and a lexical
     *  environment.
     *
     * -------------------------------------------------------------------- */
    if (car(*this).is<continuation>())
    {
      /* ---- NOTE -----------------------------------------------------------
       *
       *  If this class is constructed as make<syntactic_continuation>(...),
       *  this object until the constructor is completed, the case noted that
       *  it is the state that is not registered in the GC.
       *
       * ------------------------------------------------------------------ */
      auto const& k = car(*this).as<continuation>();

      s = k.s();
      e = k.e();
      c = compile(syntactic_context::outermost, *this, car(k.c()), cdr(k.c()));
      d = k.d();

      form() = execute();

      assert(form().is<closure>());
    }
    else
    {
      throw error(make<string>(__func__, " was called by something other than the FORK instruction"), unit);
    }
  }

  auto syntactic_continuation::current_expression() const -> const_reference
  {
    return car(form());
  }

  auto syntactic_continuation::define(const_reference name, const_reference value) -> const_reference
  {
    assert(name.is<symbol>());

    return global_environment() = make<global>(name, value) | global_environment();
  }

  auto syntactic_continuation::define(std::string const& name, const_reference value) -> const_reference
  {
    return define(intern(name), value);
  }

  auto syntactic_continuation::dynamic_environment() const -> const_reference
  {
    return cdr(form());
  }

  auto syntactic_continuation::evaluate(const_reference expression) -> value_type
  {
    c = compile(syntactic_context::none, *this, expression);

    if (is_debug_mode())
    {
      disassemble(debug_port().as<std::ostream>(), c);
    }

    return execute();
  }

  auto syntactic_continuation::execute() -> value_type
  {
    if (is_trace_mode())
    {
      return machine::execute<declaration::trace>();
    }
    else
    {
      return machine::execute();
    }
  }

  auto syntactic_continuation::fork() const -> value_type
  {
    let const module = make<syntactic_continuation>(current_continuation(), global_environment());

    module.as<syntactic_continuation>().import();
    module.as<syntactic_continuation>().build();

    return module;
  }

  auto syntactic_continuation::form() const noexcept -> const_reference
  {
    return car(*this);
  }

  auto syntactic_continuation::form() noexcept -> reference
  {
    return const_cast<reference>(std::as_const(*this).form());
  }

  auto syntactic_continuation::global_environment() const noexcept -> const_reference
  {
    return cdr(*this);
  }

  auto syntactic_continuation::global_environment() noexcept -> reference
  {
    return const_cast<reference>(std::as_const(*this).global_environment());
  }

  auto syntactic_continuation::import() -> void
  {
    define<procedure>("free-identifier=?", [this](let const& xs)
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

  auto syntactic_continuation::load(std::string const& s) -> value_type
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

      return unspecified;
    }
    else
    {
      write(debug_port(), f, "\n");

      throw file_error(make<string>("failed to open file: " + s), unit);
    }
  }

  auto syntactic_continuation::load(const_reference x) -> value_type
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
      throw file_error(make<string>(string_append(__FILE__, ":", __LINE__, ":", __func__)), unit);
    }
  }

  auto syntactic_continuation::locate(const_reference variable) -> const_reference
  {
    if (let const& binding = assq(variable, global_environment()); eq(binding, f))
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

      let const id = make<global>(variable);

      cdr(id) = id; // NOTE: Identifier is self-evaluate if is unbound.

      global_environment() = cons(id, global_environment());

      return car(global_environment());
    }
    else
    {
      return binding;
    }
  }

  auto syntactic_continuation::lookup(const_reference variable) const -> const_reference
  {
    if (let const& x = assq(variable, global_environment()); eq(x, f))
    {
      return variable.is_also<identifier>() ? variable.as<identifier>().symbol() : variable;
    }
    else
    {
      return cdr(x);
    }
  }

  auto syntactic_continuation::macroexpand(const_reference keyword, const_reference form) -> value_type
  {
    push(d, s, e, cons(make<instruction>(mnemonic::STOP), c)); // XXX ???

    s = unit;
    e = cons(cons(keyword, cdr(form)), dynamic_environment());
    c = current_expression();

    return execute();
  }

  auto operator >>(std::istream & is, syntactic_continuation & datum) -> std::istream &
  {
    datum.print("syntactic_continuation::operator >>(std::istream &, syntactic_continuation &)");
    datum.print("read new expression => ", datum.read(is));

    // sk.print("program == ", sk.program(), "current_expression is ", sk.current_expression());

    return is;
  }

  auto operator <<(std::ostream & os, syntactic_continuation & datum) -> std::ostream &
  {
    // TODO
    // Evaluate current_expression, and write the evaluation to ostream.

    return datum.write(os, "syntactic_continuation::operator <<(std::ostream &, syntactic_continuation &)\n");
  }

  auto operator <<(std::ostream & os, syntactic_continuation const& datum) -> std::ostream &
  {
    return os << magenta << "#,("
              << green << "syntactic-continuation" << reset
              << faint << " #;" << &datum << reset
              << magenta << ")" << reset;
  }

  template class configurator<syntactic_continuation>;

  template class machine<syntactic_continuation>;

  template class reader<syntactic_continuation>;

  template class writer<syntactic_continuation>;
} // namespace kernel
} // namespace meevax
