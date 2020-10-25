#ifndef INCLUDED_MEEVAX_KERNEL_WRITER_HPP
#define INCLUDED_MEEVAX_KERNEL_WRITER_HPP

#include <fstream>
#include <ostream>

#include <boost/iostreams/device/null.hpp>
#include <boost/iostreams/stream.hpp>

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
    auto write_to(std::ostream& port, Ts&&... xs) const -> decltype(port)
    {
      return (port << ... << xs) << reset;
    }

    template <typename... Ts>
    auto write(Ts&&... xs) const -> decltype(auto)
    {
      return write_to(current_output_port(), std::forward<decltype(xs)>(xs)...);
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
    auto standard_null_port() const -> auto&
    {
      static boost::iostreams::stream<boost::iostreams::null_sink> dev_null {
        boost::iostreams::null_sink()
      };

      return dev_null;
    }

    auto standard_output_port() const -> auto&
    {
      return in_batch_mode() ? standard_null_port() : std::cout;
    }

    auto standard_error_port() const -> auto&
    {
      return in_batch_mode() ? standard_null_port() : std::cerr;
    }

    auto standard_verbose_port() const -> auto&
    {
      return in_batch_mode() or not in_verbose_mode() ? standard_null_port() : std::cout;
    }

    auto standard_debug_port() const -> auto&
    {
      return in_batch_mode() or not in_debug_mode() ? standard_null_port() : std::cerr;
    }

    auto standard_interaction_port() const -> auto&
    {
      return in_batch_mode() or not in_interactive_mode() ? standard_null_port() : std::cout;
    }

    auto current_output_port() const -> decltype(auto)
    {
      return standard_output_port(); // XXX R7RS INCOMPATIBLE!
    }

    auto current_error_port() const -> decltype(auto)
    {
      return standard_error_port(); // XXX R7RS INCOMPATIBLE!
    }

    auto current_verbose_port() const -> decltype(auto)
    {
      return standard_verbose_port();
    }

    auto current_debug_port() const -> decltype(auto)
    {
      return standard_debug_port(); // XXX R7RS INCOMPATIBLE!
    }

    auto current_interaction_port() const -> decltype(auto)
    {
      return standard_interaction_port();
    }

    Define_Static_Perfect_Forwarding(open_output_file, std::ofstream);
    Define_Static_Perfect_Forwarding(open_output_string, std::stringstream);
  };
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_WRITER_HPP
