#ifndef INCLUDED_MEEVAX_KERNEL_WRITER_HPP
#define INCLUDED_MEEVAX_KERNEL_WRITER_HPP

#include <ostream>                                                // responsible
#include <sstream>

#include <boost/iostreams/device/null.hpp>
// #include <boost/iostreams/stream_buffer.hpp>
#include <boost/iostreams/stream.hpp>

#include <meevax/console/escape_sequence.hpp>
#include <meevax/kernel/boolean.hpp>

namespace meevax::kernel
{
  template <typename SK>
  class writer
  {
    friend SK;

    explicit writer()
    {}

  public:
    object debug_mode   { f };
    object quiet_mode   { f };
    object verbose_mode { f };

  public:
    template <typename... Ts>
    auto write_to(std::ostream& os, Ts&&... xs) const -> decltype(os)
    {
      return (os << ... << xs) << console::reset;
    }

    template <typename... Ts>
    auto write(Ts&&... xs) const -> decltype(auto)
    {
      return
        write_to(current_output_port(),
          std::forward<decltype(xs)>(xs)...);
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
      return quiet_mode.eqv(t) ? standard_null_port() : std::cout;
    }

    auto standard_error_port() const -> auto&
    {
      return quiet_mode.eqv(t) ? standard_null_port() : std::cerr;
    }

    auto standard_verbose_port() const -> auto&
    {
      return quiet_mode.eqv(t) or not verbose_mode.eqv(t) ? standard_null_port() : std::cout;
    }

    auto standard_debug_port() const -> auto&
    {
      return quiet_mode.eqv(t) or not debug_mode.eqv(t) ? standard_null_port() : std::cerr;
    }

  public:
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

  public:
    template <typename... Ts>
    auto open_output_file(Ts&&... xs) const
    {
      return
        std::ofstream(
          std::forward<decltype(xs)>(xs)...);
    }

    template <typename... Ts>
    auto open_output_string(Ts&&... xs) const
    {
      return
        std::ostringstream(
          std::forward<decltype(xs)>(xs)...);
    }
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_WRITER_HPP

