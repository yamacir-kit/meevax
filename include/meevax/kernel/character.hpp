#ifndef INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP
#define INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP

#include <cstdint>
#include <unordered_map>

#include <meevax/kernel/miscellaneous.hpp>
#include <meevax/kernel/object.hpp>
#include <meevax/kernel/parser.hpp>
#include <meevax/kernel/port.hpp>

namespace meevax
{
inline namespace kernel
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

    explicit character(input_port & port) // R7RS read-char
    {
      if (const auto c { port.peek() }; is_end_of_file(c))
      {
        throw read_error<eof>("exhausted input-port");
      }
      else if (0b1111'0000 < c)
      {
        push_back(port.narrow(port.get() /* & 0b0000'0111 */, 'A'));
        push_back(port.narrow(port.get() /* & 0b0011'1111 */, 'B'));
        push_back(port.narrow(port.get() /* & 0b0011'1111 */, 'C'));
        push_back(port.narrow(port.get() /* & 0b0011'1111 */, 'D'));
      }
      else if (0b1110'0000 < c)
      {
        push_back(port.narrow(port.get() /* & 0b0000'1111 */, 'A'));
        push_back(port.narrow(port.get() /* & 0b0011'1111 */, 'B'));
        push_back(port.narrow(port.get() /* & 0b0011'1111 */, 'C'));
      }
      else if (0b1100'0000 < c)
      {
        push_back(port.narrow(port.get() /* & 0b0001'1111 */, 'A'));
        push_back(port.narrow(port.get() /* & 0b0011'1111 */, 'B'));
      }
      else
      {
        push_back(port.narrow(port.get() & 0b0111'1111, 'A'));
      }
    }

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
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP
