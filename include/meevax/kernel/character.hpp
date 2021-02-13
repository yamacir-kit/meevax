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
  /* ---- R7RS 6.6. Characters -------------------------------------------------
   *
   *  Characters are objects that represent printed characters such as letters
   *  and digits. All Scheme implementations must support at least the ASCII
   *  character repertoire: that is, Unicode characters U+0000 through U+007F.
   *  Implementations may support any other Unicode characters they see fit,
   *  and may also support non-Unicode characters as well. Except as otherwise
   *  specified, the result of applying any of the following procedures to a
   *  non-Unicode character is implementation-dependent.
   *
   * ------------------------------------------------------------------------ */
  class character : public codeunit
  {
    [[deprecated]]
    auto read_codeunit(input_port &) const -> codeunit;

    auto read_char(input_port &) const -> codepoint;

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
     *  NOTE: codepoint type is std::char_traits<char>::int_type
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
    explicit character(input_port & port)
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
     *  NOTE: codepoint type is std::char_traits<char>::int_type
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

  /* ---- R7RS 6.6. Characters -------------------------------------------------
   *
   *  Characters are written using the notation #\<character> or
   *  #\<character name> or #\x<hex scalar value>.
   *
   *  The following character names must be supported by all implementations
   *  with the given values. Implementations may add other names provided they
   *  cannot be interpreted as hex scalar values preceded by x.
   *
   *    #\alarm     ; U+0007
   *    #\backspace ; U+0008
   *    #\delete    ; U+007F
   *    #\escape    ; U+001B
   *    #\newline   ; the linefeed character, U+000A
   *    #\null      ; the null character, U+0000
   *    #\return    ; the return character, U+000D
   *    #\space     ; the preferred way to write a space
   *    #\tab       ; the tab character, U+0009
   *
   *  Here are some additional examples:
   *
   *    #\a     ; lower case letter
   *    #\A     ; upper case letter
   *    #\(     ; left parenthesis
   *    #\      ; the space character
   *    #\x03BB ; λ (if character is supported)
   *    #\iota  ; ι (if character and name are supported)
   *
   *  Case is significant in #\<character>, and in #\<character name>, but not
   *  in #\x<hex scalar value>. If <character> in #\<character> is alphabetic,
   *  then any character immediately following <character> cannot be one that
   *  can appear in an identifier. This rule resolves the ambiguous case where,
   *  for example, the sequence of characters "#\space" could be taken to be
   *  either a representation of the space character or a representation of the
   *  character "#\s" followed by a representation of the symbol "pace."
   *
   *  Characters written in the #\ notation are self-evaluating. That is, they
   *  do not have to be quoted in programs.
   *
   * ------------------------------------------------------------------------ */
  auto operator <<(output_port & port, character const&) -> output_port &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP
