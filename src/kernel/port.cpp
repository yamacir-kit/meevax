#include <iomanip>

#include <meevax/kernel/port.hpp>
#include <meevax/kernel/reader.hpp>
#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace kernel
{
  void copy_ios(std::ios & from, std::ios & to)
  {
    to.copyfmt(from);
    to.clear(from.rdstate());
    to.rdbuf(from.rdbuf());
  }

  let const default_input_port  = make<standard_input_port>();
  let const default_output_port = make<standard_output_port>();
  let const default_error_port  = make<standard_error_port>();

  auto operator <<(output_port & port, standard_input_port const&) -> output_port &
  {
    return port << magenta << "#,(" << reset << "standard-input-port" << magenta << ")" << reset;
  }

  auto operator <<(output_port & port, standard_output_port const&) -> output_port &
  {
    return port << magenta << "#,(" << reset << "standard-output-port" << magenta << ")" << reset;
  }

  auto operator <<(output_port & port, standard_error_port const&) -> output_port &
  {
    return port << magenta << "#,(" << reset << "standard-error-port" << magenta << ")" << reset;
  }

  #define BOILERPLATE(TYPENAME, PORTTYPE)                                      \
  auto operator <<(output_port & port, TYPENAME const& datum) -> output_port & \
  {                                                                            \
    port << magenta << "#,(" << green << "open-" PORTTYPE << " " << datum.name << reset; \
                                                                               \
    if (not datum.is_open())                                                   \
    {                                                                          \
      port << faint << " #;closed" << reset;                                   \
    }                                                                          \
                                                                               \
    return port << magenta << ")" << reset;                                    \
  } static_assert(true)

  BOILERPLATE( input_file_port,  "input-file");
  BOILERPLATE(output_file_port, "output-file");

  #undef BOILERPLATE

  #define BOILERPLATE(TYPENAME, PORTTYPE)                                      \
  auto operator <<(output_port & port, TYPENAME const& datum) -> output_port & \
  {                                                                            \
    port << magenta << "#,(" << green << "open-" PORTTYPE;                     \
                                                                               \
    if (auto const s = datum.str(); not std::empty(s))                         \
    {                                                                          \
      port << " " << cyan << make<string>(s);                                  \
    }                                                                          \
                                                                               \
    return port << magenta << ")" << reset;                                    \
  } static_assert(true)

  BOILERPLATE( input_string_port,  "input-string");
  BOILERPLATE(output_string_port, "output-string");

  #undef BOILERPLATE
} // namespace kernel
} // namespace meevax
