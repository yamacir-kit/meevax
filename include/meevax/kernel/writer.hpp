#ifndef INCLUDED_MEEVAX_KERNEL_WRITER_HPP
#define INCLUDED_MEEVAX_KERNEL_WRITER_HPP

#include <fstream>
#include <ostream>                                                // responsible
#include <sstream>

#include <boost/iostreams/device/null.hpp>
#include <boost/iostreams/stream_buffer.hpp>

namespace meevax::kernel
{
  template <typename SK>
  class writer
    : private boost::iostreams::stream_buffer<boost::iostreams::null_sink>
  {
    friend SK;

    IMPORT(SK, quiet_is_specified)

    writer()
    {}

    bool quiet {false};

  public:
    template <typename... Ts>
    auto write(Ts&&... xs)
      -> std::ostream&
    {
      return (open_output_file() << ... << xs);
    }

    template <typename... Ts>
    auto write(std::ostream& os, Ts&&... xs)
      -> std::ostream&
    {
      return write(std::forward<decltype(xs)>(xs)...);
    }

    // from (scheme base)
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

