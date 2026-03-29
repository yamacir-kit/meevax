/*
   Copyright 2018-2025 Tatsuya Yamasaki.

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

#include <climits> // CHAR_BIT
#include <cstdint>
#include <optional>
#include <string>

namespace meevax::inline kernel
{
  struct character
  {
    using char_type = char;

    using int_type = std::char_traits<char_type>::int_type;

    static_assert(21 <= sizeof(int_type) * CHAR_BIT); // is a requirement from the maximum Unicode code point 0x10FFFF.

    static_assert(sizeof(int_type) <= 6); // is a requirement from nan-boxing.

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
        #include <meevax/unicode/property.hpp>

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

    auto static constexpr eof()
    {
      return std::char_traits<char_type>::eof();
    }

    auto static constexpr eq(int_type const& c1, int_type const& c2)
    {
      return std::char_traits<char_type>::eq_int_type(c1, c2);
    }

    auto constexpr eq(int_type const& c) const
    {
      return std::char_traits<char_type>::eq_int_type(codepoint, c);
    }

    auto constexpr digit_value() const noexcept -> std::optional<int>
    {
      switch (codepoint)
      {
      #include <meevax/unicode/digit_value.hpp>

      default:
        return std::nullopt;
      }
    }

    auto constexpr downcase() const noexcept
    {
      switch (codepoint)
      {
      #include <meevax/unicode/downcase.hpp>

      default:
        return codepoint;
      }
    }

    auto static constexpr is_ascii(int_type c)
    {
      return 0x00 <= c and c <= 0x7F;
    }

    auto static constexpr is_eof(int_type c)
    {
      return eq(eof(), c);
    }

    auto constexpr is_eof() const noexcept
    {
      return is_eof(codepoint);
    }

    auto constexpr property() const noexcept -> property_code
    {
      return codepoint;
    }

    auto constexpr upcase() const noexcept
    {
      switch (codepoint)
      {
      #include <meevax/unicode/upcase.hpp>

      default:
        return codepoint;
      }
    }

    auto utf8() const -> std::string;

    constexpr operator int_type() const
    {
      return codepoint;
    }
  };

  auto operator <<(std::ostream &, character const&) -> std::ostream &; // write
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP
