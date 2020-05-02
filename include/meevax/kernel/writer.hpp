#ifndef INCLUDED_MEEVAX_KERNEL_WRITER_HPP
#define INCLUDED_MEEVAX_KERNEL_WRITER_HPP

#include <ostream>                                                // responsible

#include <boost/iostreams/device/null.hpp>
#include <boost/iostreams/stream_buffer.hpp>

#include <meevax/utility/import.hpp>

namespace meevax::kernel
{
  template <typename SK>
  class writer
    : public boost::iostreams::stream_buffer<boost::iostreams::null_sink>
    , public std::ostream
  {
    friend SK;

    IMPORT(SK, quiet_is_specified)

    writer()
      : std::ostream {this}
    {}

  public:
    using port = std::streambuf*;

    static inline const port standard_output {std::cout.rdbuf()};
    static inline const port standard_error  {std::cerr.rdbuf()};

    operator std::ostream()
    {
      if (quiet_is_specified())
      {
        rdbuf(this);
      }
      else
      {
        rdbuf(standard_output);
      }

      return *this;
    }

    template <typename... Ts>
    auto write(Ts&&... xs)
      -> std::ostream&
    {
      return (*this << ... << xs);
    }

    template <typename... Ts>
    auto write(std::ostream& os, Ts&&... xs)
      -> std::ostream&
    {
      rdbuf(os.rdbuf());

      return write(std::forward<decltype(xs)>(xs)...);
    }
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_WRITER_HPP

