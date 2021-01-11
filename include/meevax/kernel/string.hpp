#ifndef INCLUDED_MEEVAX_KERNEL_STRING_HPP
#define INCLUDED_MEEVAX_KERNEL_STRING_HPP

#include <meevax/kernel/character.hpp>
#include <meevax/kernel/pair.hpp>
#include <meevax/kernel/port.hpp>

namespace meevax
{
inline namespace kernel
{
  struct string
    : public virtual pair
  {
    auto write_string() const -> bytestring;

    auto write_string(output_port&) const -> output_port &;

    auto write_string(let const&) const -> output_port &;

    operator bytestring() const
    {
      return write_string();
    }
  };

  bool operator ==(string const&, string const&);

  auto operator <<(output_port &, string const&) -> output_port &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_STRING_HPP
