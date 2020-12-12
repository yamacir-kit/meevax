#ifndef INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP
#define INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP

#include <cstdint>
#include <unordered_map>

#include <meevax/kernel/miscellaneous.hpp>
#include <meevax/kernel/object.hpp>
#include <meevax/kernel/port.hpp>

namespace meevax
{
inline namespace kernel
{
  /* ---- Character --------------------------------------------------------- */

  auto codepoint_to_codeunit(std::uint_least32_t) -> std::string;

  auto codeunit_to_codepoint(std::string const&) -> std::uint_least32_t;

  auto read_codeunit(input_port &) -> std::string;
  auto peek_codeunit(input_port &) -> std::string;

  struct character
    : public std::string
  {
    explicit character(char ascii)
      : std::string(1, ascii)
    {}

    explicit character(std::uint32_t codepoint) // R7RS integer->char
      : std::string { codepoint_to_codeunit(codepoint) }
    {}

    explicit character(input_port & port) // R7RS read-char
      : std::string { read_codeunit(port) }
    {}

    template <typename... Ts>
    explicit constexpr character(Ts&&... xs)
      : std::string { std::forward<decltype(xs)>(xs)... }
    {}

    virtual ~character() = default;

    decltype(auto) codepoint() const // R7RS char->integer
    {
      return codeunit_to_codepoint(*this);
    }

    /* ---- R7RS write-char ------------------------------------------------- */

    auto write_char() const -> std::string const&;

    auto write_char(output_port &) const -> output_port &;

    auto write_char(let const&) const -> output_port &;
  };

  auto operator <<(output_port & port, character const&) -> output_port &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP
