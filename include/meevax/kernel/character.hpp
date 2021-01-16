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

  // auto codepoint_to_codeunit(std::uint_least32_t) -> bytestring;

  auto codeunit_to_codepoint(bytestring const&) -> std::uint_least32_t;

  auto read_codeunit(input_port &) -> bytestring;
  auto peek_codeunit(input_port &) -> bytestring;

  struct character
    : public bytestring
  {
    explicit character(char ascii)
      : bytestring(1, ascii)
    {}

    explicit character(std::uint32_t codepoint) // R7RS integer->char
      : bytestring { codepoint_to_codeunit(codepoint) }
    {}

    explicit character(input_port & port) // R7RS read-char
      : bytestring { read_codeunit(port) }
    {}

    template <typename... Ts>
    explicit constexpr character(Ts&&... xs)
      : bytestring { std::forward<decltype(xs)>(xs)... }
    {}

    virtual ~character() = default;

    decltype(auto) codepoint() const // R7RS char->integer
    {
      return codeunit_to_codepoint(*this);
    }

    /* ---- R7RS write-char ------------------------------------------------- */

    auto write_char() const -> bytestring const&;

    auto write_char(output_port &) const -> output_port &;

    auto write_char(let const&) const -> output_port &;
  };

  auto operator <<(output_port & port, character const&) -> output_port &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP
