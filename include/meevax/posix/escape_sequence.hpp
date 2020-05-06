#ifndef INCLUDED_MEEVAX_POSIX_ESCAPE_SEQUENCE_HPP
#define INCLUDED_MEEVAX_POSIX_ESCAPE_SEQUENCE_HPP

#include <iostream>

#include <meevax/unix/teletype.hpp>

namespace meevax::posix
{
  template <typename... Ts>
  auto escape_sequence(std::ostream& os, Ts&&... xs)
    -> auto&
  {
    return unix::is_tty(os) ? (os << "\x1b[" << ... << xs) : os;
  }

  namespace attribute
  {
    // static constexpr auto* normal      {"\x1b[0m"};

    auto normal = [](std::ostream& os) -> auto&
    {
      return escape_sequence(os, "0m");
    };

    static constexpr auto* bold        {"\x1b[1m"};
    static constexpr auto* faint       {"\x1b[2m"};
    static constexpr auto* italic      {"\x1b[3m"}; // Not widely supported. Sometimes treated as inverse.
    static constexpr auto* underline   {"\x1b[4m"};
    static constexpr auto* slow_blink  {"\x1b[5m"}; // Less than 150 per minite.
    static constexpr auto* rapid_blink {"\x1b[6m"}; // More than 150 per minite. Not widely supported.
    static constexpr auto* reverse     {"\x1b[7m"};
    static constexpr auto* conceal     {"\x1b[8m"}; // Not widely supported.

    constexpr auto* increased_intensity {bold};
    constexpr auto* decreased_intensity {faint};
  }

  namespace color
  {
    inline namespace foreground
    {
      static constexpr auto* black   {"\x1b[30m"};
      static constexpr auto* red     {"\x1b[31m"};
      static constexpr auto* green   {"\x1b[32m"};
      static constexpr auto* yellow  {"\x1b[33m"};
      static constexpr auto* blue    {"\x1b[34m"};
      static constexpr auto* magenta {"\x1b[35m"};
      static constexpr auto* cyan    {"\x1b[36m"};
      static constexpr auto* white   {"\x1b[37m"};
    }

    namespace background
    {
      static constexpr auto* black   {"\x1b[40m"};
      static constexpr auto* red     {"\x1b[41m"};
      static constexpr auto* green   {"\x1b[42m"};
      static constexpr auto* yellow  {"\x1b[43m"};
      static constexpr auto* blue    {"\x1b[44m"};
      static constexpr auto* magenta {"\x1b[45m"};
      static constexpr auto* cyan    {"\x1b[46m"};
      static constexpr auto* white   {"\x1b[47m"};
    }
  }

  // TODO Rename to "highlight_as"
  namespace highlight
  {
    constexpr auto* comment {attribute::faint};
    constexpr auto* datum   {color::cyan};
    constexpr auto* syntax  {color::magenta};
    constexpr auto* type    {color::green};
    constexpr auto* warning {color::yellow};
  }
} // namespace meevax::posix

#endif // INCLUDED_MEEVAX_POSIX_ESCAPE_SEQUENCE_HPP

