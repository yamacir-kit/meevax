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
    /* ---- R7RS 6.13.2. Input -------------------------------------------------
     *
     *  (read-string k)                                               procedure
     *  (read-string k port)                                          procedure
     *
     *  Reads the next k characters, or as many as are available before the end
     *  of file, from the textual input port into a newly allocated string in
     *  left-to-right order and returns the string. If no characters are
     *  available before the end of file, an end-of-file object is returned.
     *
     * ---------------------------------------------------------------------- */
    // TODO string(input_port &);

    auto write_string() const -> std::string;

    auto write_string(output_port&) const -> output_port &;

    auto write_string(let const&) const -> output_port &;

    operator std::string() const
    {
      return write_string();
    }
  };

  bool operator ==(string const&, string const&);

  auto operator <<(output_port &, string const&) -> output_port &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_STRING_HPP
