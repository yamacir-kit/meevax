#ifndef INCLUDED_MEEVAX_KERNEL_MISCELLANEOUS_HPP
#define INCLUDED_MEEVAX_KERNEL_MISCELLANEOUS_HPP

#include <meevax/kernel/pair.hpp>
#include <meevax/kernel/port.hpp>

namespace meevax
{
inline namespace kernel
{
  /* ---- End-of-File ------------------------------------------------------- */

  struct eof {};

  let extern const eof_object;

  auto operator <<(output_port & port, eof const&) -> decltype(port);

  /* ---- End-of-String ----------------------------------------------------- */

  struct eos {};

  let extern const eos_object;

  auto operator <<(output_port & port, eos const&) -> decltype(port);
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_MISCELLANEOUS_HPP
