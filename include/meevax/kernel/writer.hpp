#ifndef INCLUDED_MEEVAX_KERNEL_WRITER_HPP
#define INCLUDED_MEEVAX_KERNEL_WRITER_HPP

#include <meevax/kernel/port.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename SK>
  class writer
  {
    friend SK;

    explicit writer()
    {}

    IMPORT(SK, in_batch_mode, const);
    IMPORT(SK, in_debug_mode, const);
    IMPORT(SK, in_interactive_mode, const);
    IMPORT(SK, in_verbose_mode, const);

  public:
    template <typename... Ts>
    auto write_to(output_port & port, Ts&&... xs) const -> output_port &
    {
      return (port << ... << xs) << reset;
    }

    template <typename... Ts>
    auto write_to(let const& x, Ts&&... xs) const -> decltype(auto)
    {
      return write_to(x.as<output_port>(), std::forward<decltype(xs)>(xs)...);
    }

    template <typename... Ts>
    auto write(Ts&&... xs) const -> decltype(auto)
    {
      return write_to(default_output_port, std::forward<decltype(xs)>(xs)...);
    }

    template <typename... Ts>
    auto write_line(Ts&&... xs) const -> decltype(auto)
    {
      return write(std::forward<decltype(xs)>(xs)..., '\n');
    }

    auto newline() const -> decltype(auto)
    {
      return write_line();
    }

  public:
    // TODO MOVE INTO writer.cpp
    let standard_null_port() const
    {
      let static port = make<output_file_port>("/dev/null");
      return port;
    }

    auto standard_verbose_port() const -> decltype(auto)
    {
      return in_verbose_mode() ? default_output_port : standard_null_port();
    }

    auto standard_debug_port() const -> decltype(auto)
    {
      return in_debug_mode() ? default_error_port : standard_null_port();
    }

    auto standard_interaction_port() const -> decltype(auto)
    {
      return in_interactive_mode() ? default_output_port : standard_null_port();
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_WRITER_HPP
