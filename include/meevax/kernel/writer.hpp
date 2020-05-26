#ifndef INCLUDED_MEEVAX_KERNEL_WRITER_HPP
#define INCLUDED_MEEVAX_KERNEL_WRITER_HPP

#include <fstream>
#include <ostream>                                                // responsible
#include <sstream>

#include <boost/iostreams/device/null.hpp>
#include <boost/iostreams/stream.hpp>

#include <meevax/console/escape_sequence.hpp>
#include <meevax/kernel/boolean.hpp>
#include <meevax/utility/perfect_forward.hpp>

namespace meevax::kernel
{
  template <typename SK>
  class writer
  {
    friend SK;

    explicit writer()
    {}

    Import_Const(SK, debugging);
    Import_Const(SK, interactive);
    Import_Const(SK, quiet);
    Import_Const(SK, verbose);

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
      return quiet() ? standard_null_port() : std::cout;
    }

    auto standard_error_port() const -> auto&
    {
      return quiet() ? standard_null_port() : std::cerr;
    }

    auto standard_verbose_port() const -> auto&
    {
      return quiet() or not verbose() ? standard_null_port() : std::cout;
    }

    auto standard_debug_port() const -> auto&
    {
      return quiet() or not debugging() ? standard_null_port() : std::cerr;
    }

    auto standard_interaction_port() const -> auto&
    {
      return quiet() or not interactive() ? standard_null_port() : std::cout;
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

    auto current_interaction_port() const -> decltype(auto)
    {
      return standard_interaction_port();
    }

  public:
    Static_Perfect_Forward(open_output_file, std::ofstream);
    Static_Perfect_Forward(open_output_string, std::stringstream);
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_WRITER_HPP

