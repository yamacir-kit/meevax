#ifndef INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP
#define INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP

#include <cstdint>
#include <unordered_map>

#include <meevax/kernel/object.hpp>
#include <meevax/kernel/port.hpp>

namespace meevax { inline namespace kernel
{
  /* ---- Character --------------------------------------------------------- */

  auto encode(std::uint_least32_t) -> std::string;

  auto decode(std::string const&) -> std::uint_least32_t;

  struct character
    : public std::string
  {
    explicit character(char ascii)
      : std::string(1, ascii)
    {}

    explicit character(std::uint32_t codepoint) // R7RS integer->char
      : std::string { encode(codepoint) }
    {}

    template <typename... Ts>
    explicit constexpr character(Ts&&... xs)
      : std::string { std::forward<decltype(xs)>(xs)... }
    {}

    virtual ~character() = default;

    decltype(auto) codepoint() const // R7RS char->integer
    {
      return decode(*this);
    }

    /* ---- R7RS write-char ------------------------------------------------- */

    auto write_char() const -> std::string const&;

    auto write_char(output_port &) const -> output_port &;

    auto write_char(let const&) const -> output_port &;
  };

  auto operator <<(std::ostream& port, const character&) -> decltype(port);
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP
