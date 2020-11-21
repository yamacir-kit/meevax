#ifndef INCLUDED_MEEVAX_KERNEL_WRITER_HPP
#define INCLUDED_MEEVAX_KERNEL_WRITER_HPP

#include <fstream>
#include <ostream>

#include <meevax/kernel/object.hpp>

namespace meevax { inline namespace kernel
{
  template <typename SK>
  class writer
  {
    friend SK;

    explicit writer()
    {}

    Import_Const(SK, in_batch_mode);
    Import_Const(SK, in_debug_mode);
    Import_Const(SK, in_interactive_mode);
    Import_Const(SK, in_verbose_mode);

  public:
    template <typename... Ts>
    auto write_to(std::ostream& port, Ts&&... xs) const -> decltype(auto)
    {
      return (port << ... << xs) << reset;
    }

    template <typename... Ts>
    auto write_to(const object& x, Ts&&... xs) const -> decltype(auto)
    {
      return write_to(x.as<output_port>(), std::forward<decltype(xs)>(xs)...);
    }

    template <typename... Ts>
    auto write(Ts&&... xs) const -> decltype(auto)
    {
      return write_to(standard_output_port(), std::forward<decltype(xs)>(xs)...);
    }

    template <typename... Ts>
    auto writeln(Ts&&... xs) const -> decltype(auto)
    {
      return write(std::forward<decltype(xs)>(xs)..., '\n');
    }

    auto newline() const -> decltype(auto)
    {
      return writeln();
    }

  public:
    // TODO MOVE INTO writer.cpp
    let standard_null_port() const
    {
      let static port = make<output_port>("/dev/null");
      return port;
    }

    // TODO MOVE INTO writer.cpp
    let standard_output_port() const
    {
      let static port = make<output_port>("/dev/stdout", std::cout);
      return in_batch_mode() ? standard_null_port() : port;
    }

    // TODO MOVE INTO writer.cpp
    let standard_error_port() const
    {
      let static port = make<output_port>("/dev/stderr", std::cerr);
      return in_batch_mode() ? standard_null_port() : port;
    }

    auto standard_verbose_port() const -> decltype(auto)
    {
      return in_verbose_mode() ? standard_output_port() : standard_null_port();
    }

    auto standard_debug_port() const -> decltype(auto)
    {
      return in_debug_mode() ? standard_error_port() : standard_null_port();
    }

    auto standard_interaction_port() const -> decltype(auto)
    {
      return in_interactive_mode() ? standard_output_port() : standard_null_port();
    }
  };
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_WRITER_HPP
