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
    return notate(name, syntactic_environment()).as<absolute>().strip();
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
    assert(is_identifier(name));

    global_environment() = make<absolute>(name, value) | global_environment();
  }

  auto environment::define(std::string const& name, const_reference value) -> void
  {
    define(intern(name), value);
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
    auto dump = std::make_tuple(std::exchange(s, unit),
                                std::exchange(e, unit),
                                std::exchange(c, compile(context::none, *this, expression, syntactic_environment())),
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

  auto environment::global_environment() const noexcept -> const_reference
  {
    return second;
  }

  auto environment::global_environment() noexcept -> reference
  {
    return second;
  }

  auto environment::import() -> void
  {
    define<procedure>("set-batch!",       [this](let const& xs, auto&&) { return batch       = car(xs); });
    define<procedure>("set-debug!",       [this](let const& xs, auto&&) { return debug       = car(xs); });
    define<procedure>("set-interactive!", [this](let const& xs, auto&&) { return interactive = car(xs); });
    define<procedure>("set-prompt!",      [this](let const& xs, auto&&) { return prompt      = car(xs); });
    define<procedure>("set-trace!",       [this](let const& xs, auto&&) { return trace       = car(xs); });
    define<procedure>("set-verbose!",     [this](let const& xs, auto&&) { return verbose     = car(xs); });
  }

  auto environment::is_identifier(const_reference x) -> bool
  {
    return x.is<symbol>() or x.is_also<absolute>() or x.is<syntactic_closure>();
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

  auto environment::syntactic_environment() const noexcept -> const_reference
  {
    return first;
  }

  auto environment::syntactic_environment() noexcept -> reference
  {
    return first;
  }

  auto environment::notate(const_reference variable, const_reference syntactic_environment) const -> object
  {
    if (not is_identifier(variable))
    {
      return f;
    }
    else if (let const& notation = machine::notate(variable, syntactic_environment); select(notation))
    {
      return notation;
    }
    else
    {
      return assq(variable, global_environment());
    }
  }

  auto environment::notate(const_reference variable, const_reference syntactic_environment) -> object
  {
    if (not is_identifier(variable))
    {
      return f;
    }
    if (let const& notation = std::as_const(*this).notate(variable, syntactic_environment); select(notation))
    {
      return notation;
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
      return reserve(variable);
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
