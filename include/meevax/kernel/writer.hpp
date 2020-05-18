#ifndef INCLUDED_MEEVAX_KERNEL_WRITER_HPP
#define INCLUDED_MEEVAX_KERNEL_WRITER_HPP

#include <ostream>                                                // responsible
#include <sstream>

#include <boost/iostreams/device/null.hpp>
// #include <boost/iostreams/stream_buffer.hpp>
#include <boost/iostreams/stream.hpp>

#include <meevax/console/escape_sequence.hpp>
#include <meevax/kernel/boolean.hpp>
#include <type_traits>

namespace meevax::kernel
{
  static boost::iostreams::stream<boost::iostreams::null_sink> bucket {
    boost::iostreams::null_sink()
  };

  template <typename SK>
  class writer
  {
    friend SK;

    explicit writer()
    {}

  public:
    object debugging {f};
    object quiet {f};
    // verbosely

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
    auto standard_output_port() const -> auto&
    {
      return quiet.eqv(t) ? bucket : std::cout;
    }

    auto standard_error_port() const -> auto&
    {
      return quiet.eqv(t) ? bucket : std::cerr;
    }

    auto standard_debug_port() const -> auto&
    {
      return quiet.eqv(t) or not debugging.eqv(t) ? bucket : std::cerr;
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

