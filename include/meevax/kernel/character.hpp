#ifndef INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP
#define INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP

#include <cstdint>
#include <unordered_map>

#include <meevax/kernel/miscellaneous.hpp>
#include <meevax/kernel/object.hpp>
#include <meevax/kernel/port.hpp>
#include <meevax/string/unicode.hpp>

namespace meevax
{
inline namespace kernel
{
  /* ---- Character --------------------------------------------------------- */

  class character
    : public codeunit
  {
    auto read_codeunit(input_port &) const -> codeunit;

  public:
    /* ---- R7RS 6.6. Characters -----------------------------------------------
     *
     *  (char->integer char)                                          procedure
     *  (integer->char n)                                             procedure
     *
     *  Given a Unicode character, char->integer returns an exact integer
     *  between 0 and #xD7FF or between #xE000 and #x10FFFF which is equal to
     *  the Unicode scalar value of that character. Given a non-Unicode
     *  character, it returns an exact integer greater than #x10FFFF. This is
     *  true independent of whether the implementation uses the Unicode
     *  representation internally.
     *
     *  Given an exact integer that is the value returned by a character when
     *  char->integer is applied to it, integer->char returns that character.
     *
     * ---------------------------------------------------------------------- */
    explicit character(codepoint const value)
      : codeunit { codepoint_to_codeunit(value) }
    {}

    /* ---- R7RS 6.13.2. Input -------------------------------------------------
     *
     *  (read-char)                                                   procedure
     *  (read-char port)                                              procedure
     *
     *  Returns the next character available from the textual input port,
     *  updating the port to point to the following character. If no more
     *  characters are available, an end-of-file object is returned.
     *
     * ---------------------------------------------------------------------- */
    explicit character(input_port & port) // R7RS read-char
      : codeunit { read_codeunit(port) }
    {}

    template <typename... Ts>
    explicit constexpr character(Ts&&... xs)
      : codeunit { std::forward<decltype(xs)>(xs)... }
    {}

    virtual ~character() = default;

    /* ---- R7RS 6.6. Characters -----------------------------------------------
     *
     *  (char->integer char)                                          procedure
     *  (integer->char n)                                             procedure
     *
     *  Given a Unicode character, char->integer returns an exact integer
     *  between 0 and #xD7FF or between #xE000 and #x10FFFF which is equal to
     *  the Unicode scalar value of that character. Given a non-Unicode
     *  character, it returns an exact integer greater than #x10FFFF. This is
     *  true independent of whether the implementation uses the Unicode
     *  representation internally.
     *
     *  Given an exact integer that is the value returned by a character when
     *  char->integer is applied to it, integer->char returns that character.
     *
     * ---------------------------------------------------------------------- */
    operator codepoint() const
    {
      return codeunit_to_codepoint(*this);
    }

    /* ---- R7RS 6.13.3. Output ------------------------------------------------
     *
     *  (write-char char)                                             procedure
     *  (write-char char port)                                        procedure
     *
     *  Writes the character char (not an external representation of the
     *  character) to the given textual output port and returns an unspecified
     *  value.
     *
     * --------------------------------------------------------------------- */
    auto write_char(output_port &) const -> output_port &;
  };

  auto operator <<(output_port & port, character const&) -> output_port &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP
