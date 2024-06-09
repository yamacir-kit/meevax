/*
   Copyright 2018-2024 Tatsuya Yamasaki.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

#ifndef INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP
#define INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP

#include <cstdint>
#include <optional>
#include <string>

namespace meevax
{
inline namespace kernel
{
  struct character
  {
    using char_type = char;

    using int_type = std::char_traits<char_type>::int_type;

    /*
       21 <= sizeof(int_type) * 8 is a request from the maximum Unicode code
       point 0x10FFFF. sizeof(int_type) < 6 is a requirement from nan-boxing.
    */
    static_assert(21 <= sizeof(int_type) * 8 and sizeof(int_type) <= 6);

    int_type codepoint;

    struct property_code
    {
      enum value_type
      {
        Cc, // Other, Control
        Cf, // Other, Format
        Cn, // Other, Not Assigned (no characters in the file have this property)
        Co, // Other, Private Use
        Cs, // Other, Surrogate
        Ll, // Letter, Lowercase
        Lm, // Letter, Modifier
        Lo, // Letter, Other
        Lt, // Letter, Titlecase
        Lu, // Letter, Uppercase
        Mc, // Mark, Spacing Combining
        Me, // Mark, Enclosing
        Mn, // Mark, Non-Spacing
        Nd, // Number, Decimal Digit
        Nl, // Number, Letter
        No, // Number, Other
        Pc, // Punctuation, Connector
        Pd, // Punctuation, Dash
        Pe, // Punctuation, Close
        Pf, // Punctuation, Final quote (may behave like Ps or Pe depending on usage)
        Pi, // Punctuation, Initial quote (may behave like Ps or Pe depending on usage)
        Po, // Punctuation, Other
        Ps, // Punctuation, Open
        Sc, // Symbol, Currency
        Sk, // Symbol, Modifier
        Sm, // Symbol, Math
        So, // Symbol, Other
        Zl, // Separator, Line
        Zp, // Separator, Paragraph
        Zs, // Separator, Space
      } value;

      constexpr property_code(int_type codepoint)
        : value { to_value_type(codepoint) }
      {}

      static constexpr auto to_value_type(int_type codepoint) noexcept -> value_type
      {
        switch (codepoint)
        {
        #if __has_include(<meevax/unicode/property.hpp>)
        #include <meevax/unicode/property.hpp>
        #endif

        default:
          return Cn;
        }
      }

      template <typename... Ts>
      constexpr auto is_any_of(Ts&&... xs) const noexcept
      {
        return ((value == xs) or ...);
      }

      constexpr auto is_letter() const noexcept
      {
        return is_any_of(Ll, Lm, Lo, Lt, Lu);
      }

      constexpr auto is_lower_case() const noexcept
      {
        return value == Ll;
      }

      constexpr auto is_numeric() const noexcept
      {
        return value == Nd;
      }

      constexpr auto is_upper_case() const noexcept
      {
        return value == Lu;
      }

      constexpr auto is_whitespace() const noexcept
      {
        return value == Zs;
      }
    };

    character() = default;

    explicit constexpr character(int_type codepoint)
      : codepoint { codepoint }
    {}

    static constexpr auto eof()
    {
      return std::char_traits<char_type>::eof();
    }

    static constexpr auto eq(int_type const& c1, int_type const& c2)
    {
      return std::char_traits<char_type>::eq_int_type(c1, c2);
    }

    constexpr auto eq(int_type const& c) const
    {
      return std::char_traits<char_type>::eq_int_type(codepoint, c);
    }

    constexpr auto digit_value() const noexcept -> std::optional<int>
    {
      switch (codepoint)
      {
      #if __has_include(<meevax/unicode/digit_value.hpp>)
      #include <meevax/unicode/digit_value.hpp>
      #endif

      default:
        return std::nullopt;
      }
    }

    constexpr auto downcase() const noexcept
    {
      switch (codepoint)
      {
      #if __has_include(<meevax/unicode/downcase.hpp>)
      #include <meevax/unicode/downcase.hpp>
      #endif

      default:
        return codepoint;
      }
    }

    static constexpr auto is_ascii(int_type c)
    {
      return 0x00 <= c and c <= 0x7F;
    }

    static constexpr auto is_eof(int_type c)
    {
      return eq(eof(), c);
    }

    constexpr auto is_eof() const noexcept
    {
      return is_eof(codepoint);
    }

    constexpr auto property() const noexcept -> property_code
    {
      return codepoint;
    }

    constexpr auto upcase() const noexcept
    {
      switch (codepoint)
      {
      #if __has_include(<meevax/unicode/upcase.hpp>)
      #include <meevax/unicode/upcase.hpp>
      #endif

      default:
        return codepoint;
      }
    }

    constexpr operator int_type() const
    {
      return codepoint;
    }

    explicit operator std::string() const; // write-char (for display)
  };

  auto operator <<(std::ostream &, character const&) -> std::ostream &; // write
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP
