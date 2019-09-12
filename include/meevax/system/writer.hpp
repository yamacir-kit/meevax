#ifndef INCLUDED_MEEVAX_SYSTEM_WRITER_HPP
#define INCLUDED_MEEVAX_SYSTEM_WRITER_HPP

/*
 * This header is responsible for including <ostream> and <string>.
 */
#include <ostream>
#include <string>

namespace meevax::system
{
  template <typename OutputStream, typename... Objects>
  decltype(auto) write(OutputStream&& os, Objects&&... objects)
  {
    (os << ... << objects);
    return os;
  }

  namespace attribute
  {
    static constexpr auto
      normal      {"\x1b[0m"},
      bold        {"\x1b[1m"},
      faint       {"\x1b[2m"},
      italic      {"\x1b[3m"}, // Not widely supported. Sometimes treated as inverse.
      underline   {"\x1b[4m"},
      slow_blink  {"\x1b[5m"}, // Less than 150 per minite.
      rapid_blink {"\x1b[6m"}, // More than 150 per minite. Not widely supported.
      reverse     {"\x1b[7m"},
      conceal     {"\x1b[8m"}; // Not widely supported.

    constexpr auto // alias
      increased_intensity {bold},
      decreased_intensity {faint};
  }

  namespace color
  {
    inline namespace foreground
    {
      static constexpr auto
        black   {"\x1b[30m"},
        red     {"\x1b[31m"},
        green   {"\x1b[32m"},
        yellow  {"\x1b[33m"},
        blue    {"\x1b[34m"},
        magenta {"\x1b[35m"},
        cyan    {"\x1b[36m"},
        white   {"\x1b[37m"};
    }

    namespace background
    {
      static constexpr auto
        black   {"\x1b[40m"},
        red     {"\x1b[41m"},
        green   {"\x1b[42m"},
        yellow  {"\x1b[43m"},
        blue    {"\x1b[44m"},
        magenta {"\x1b[45m"},
        cyan    {"\x1b[46m"},
        white   {"\x1b[47m"};
    }
  }

  namespace highlight
  {
    constexpr auto
      comment {attribute::faint},
      constructor {color::green},
      simple_datum {color::cyan},
      syntax {color::magenta};
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_WRITER_HPP

