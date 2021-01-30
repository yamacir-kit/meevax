#include <iomanip>

#include <meevax/kernel/port.hpp>
#include <meevax/kernel/reader.hpp>
#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace kernel
{
  auto operator <<(output_port & port, standard_input const&) -> output_port &
  {
    return port << magenta << "#,(" << reset << "standard-input-port" << magenta << ")" << reset;
  }

  auto operator <<(output_port & port, standard_output const&) -> output_port &
  {
    return port << magenta << "#,(" << reset << "standard-output-port" << magenta << ")" << reset;
  }

  auto operator <<(output_port & port, standard_error const&) -> output_port &
  {
    return port << magenta << "#,(" << reset << "standard-error-port" << magenta << ")" << reset;
  }

  #define BOILERPLATE(TYPENAME, PORTTYPE)                                      \
  auto operator<<(std::ostream& port, const TYPENAME& datum) -> decltype(port) \
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
  auto operator<<(std::ostream& port, const TYPENAME& datum) -> decltype(port) \
  {                                                                            \
    port << magenta << "#,(" << green << "open-" PORTTYPE;                     \
                                                                               \
    if (const auto s { datum.str() }; not std::empty(s))                       \
    {                                                                          \
      port << " " << cyan << make_string(s);                                   \
    }                                                                          \
                                                                               \
    return port << magenta << ")" << reset;                                    \
  } static_assert(true)

  BOILERPLATE( input_string_port,  "input-string");
  BOILERPLATE(output_string_port, "output-string");

  #undef BOILERPLATE
} // namespace kernel
} // namespace meevax
