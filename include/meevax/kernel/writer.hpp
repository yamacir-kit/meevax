#ifndef INCLUDED_MEEVAX_KERNEL_WRITER_HPP
#define INCLUDED_MEEVAX_KERNEL_WRITER_HPP

#include <ostream>                                                // responsible
#include <sstream>

#include <boost/iostreams/device/null.hpp>
#include <boost/iostreams/stream_buffer.hpp>

#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/port.hpp>

namespace meevax::kernel
{
  template <typename SK>
  class writer
    : private boost::iostreams::stream_buffer<boost::iostreams::null_sink>
  {
    friend SK;

    writer()
    {}

    IMPORT(SK, quiet_is_specified)

  protected:
    bool debug {false};
    bool quiet {false};

  public:
    template <typename... Ts>
    auto write(std::ostream& os, Ts&&... xs)
      -> std::ostream&
    {
      return (os << ... << xs);
    }

    template <typename... Ts>
    auto write(Ts&&... xs)
      -> std::ostream&
    {
      return
        write(
          current_output_port(),
          std::forward<decltype(xs)>(xs)...);
    }

  public:
    auto current_output_port() const
    {
      return
        std::ostream(
          quiet ? this : std::cout.rdbuf());
    }

    auto current_error_port() const
    {
      return
        std::ostream(
          quiet ? this : std::cerr.rdbuf());
    }

    auto current_debug_port() const
    {
      return
        std::ostream(
          quiet or not debug ? this : std::cerr.rdbuf());
    }

  public:
    template <typename S>
    auto open_output_file(S&& s) const
    {
      return
        std::ofstream(
          std::forward<decltype(s)>(s));
    }

    auto open_output_string() const
    {
      return
        std::ostringstream();
    }
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_WRITER_HPP

