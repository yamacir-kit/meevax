#include <meevax/kernel/miscellaneous.hpp>
#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace kernel
{
  let const eof_object = make<eof>();

  auto operator <<(output_port & port, eof const&) -> output_port &
  {
    return port << magenta << "#,(" << green << "eof-object" << magenta << ")" << reset;
  }

  let const eos_object = make<eos>();

  auto operator <<(output_port & port, eos const&) -> output_port &
  {
    return port << magenta << "#,(" << green << "eos-object" << magenta << ")" << reset;
  }
} // namespace kernel
} // namespace meevax
