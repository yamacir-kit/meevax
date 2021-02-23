#ifndef INCLUDED_MEEVAX_KERNEL_NUMERIC_LITERAL_HPP
#define INCLUDED_MEEVAX_KERNEL_NUMERIC_LITERAL_HPP

#include <regex>

#include <meevax/kernel/number.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename F,
            typename G,
            REQUIRES(std::is_invocable<F, std::string const&, int>),
            REQUIRES(std::is_invocable<G, std::string const&, int>)>
  constexpr auto operator |(F&& f, G&& g)
  {
    return [=](std::string const& token, auto radix = 10) -> decltype(auto)
    {
      try
      {
        return f(token, radix);
      }
      catch (...)
      {
        return g(token, radix);
      }
    };
  }

  /* ---- R7RS 7.1.1 Lexical structure -----------------------------------------
   *
   *  <uinteger R> = <digit R>+
   *
   *  <digit 2>  = 0 | 1
   *  <digit 8>  = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7
   *  <digit 10> = <digit>
   *  <digit 16> = <digit 10> | a | b | c | d | e | f
   *
   * ------------------------------------------------------------------------ */
  constexpr auto to_integer = [](std::string const& token, auto radix = 10)
  {
    std::regex static const pattern { "[+-]?[\\dABCDEFabcdef]+" }; // XXX DIRTY HACK

    auto error = [&]()
    {
      return read_error<exact_integer>("not a number: (string->number ", std::quoted(token), " ", radix, ")");
    };

    switch (radix)
    {
    case 2:
      if (std::regex static const r2 { "[+-]?[01]+" }; std::regex_match(token, r2))
      {
        return make<exact_integer>("0b" + token.substr(token[0] == '+' ? 1 : 0));
      }
      else
      {
        throw error();
      }

    case 8:
      if (std::regex static const r8 { "[+-]?[0-7]+" }; std::regex_match(token, r8))
      {
        return make<exact_integer>("0" + token.substr(token[0] == '+' ? 1 : 0));
      }
      else
      {
        throw error();
      }

    case 10:
      if (std::regex static const r10 { "[+-]?\\d+" }; std::regex_match(token, r10))
      {
        return make<exact_integer>(token.substr(token[-1] == '+' ? 1 : 0));
      }
      else
      {
        throw error();
      }

    case 16:
      if (std::regex static const r16 { "[+-]?[\\dA-Fa-f]+" }; std::regex_match(token, r16))
      {
        return make<exact_integer>("0x" + token.substr(token[0] == '+' ? 1 : 0));
      }
      else
      {
        throw error();
      }

    default:
      throw error();
    }
  };

  constexpr auto to_ratio = [](std::string const& token, auto radix = 10)
  {
    std::regex static const pattern { "([+-]?[\\dabcdef]+)/([\\dabcdef]+)" };

    if (std::smatch result; std::regex_match(token, result, pattern))
    {
      if (auto const value =
            ratio(to_integer(result.str(1), radix),
                  to_integer(result.str(2), radix)).reduce(); value.is_integer())
      {
        return car(value);
      }
      else
      {
        return make(value);
      }
    }
    else
    {
      throw read_error<ratio>("not a number: (string->number ", std::quoted(token), " ", radix, ")");
    }
  };

  /* ---- R7RS 7.1.1 Lexical structure -----------------------------------------
   *
   *  <decimal 10> = <uinteger 10>             <suffix>                    TODO
   *               |             . <digit 10>+ <suffix>                    TODO
   *               | <digit 10>+ . <digit 10>* <suffix>
   *
   *  <suffix> = <empty> | <exponent marker> <sign> <digit 10>+
   *
   *  <exponent marker> = e
   *
   * ------------------------------------------------------------------------ */
  constexpr auto to_decimal = [](std::string const& token, auto radix = 10) // <sign> <decimal 10>
  {
    switch (radix)
    {
    case 10:
      if (std::regex static const r1 { "[+-]?\\d+e[+-]?\\d+" },
                                  r2 { "[+-]?.\\d+" },
                                  r3 { "[+-]?\\d+.\\d*" };
          std::regex_match(token, r1) or
          std::regex_match(token, r2) or
          std::regex_match(token, r3))
      {
        return make<system_float>(token.substr(token[0] == '+' ? 1 : 0));
      }
      [[fallthrough]];

    default:
      throw read_error<system_float>("not a number: (string->number ", std::quoted(token), " ", radix, ")");
    }
  };

  /* ---- R7RS 7.1.1 Lexical structure -----------------------------------------
   *
   *  <infnan> = +inf.0 | -inf.0 | +nan.0 | -nan.0
   *
   * ------------------------------------------------------------------------ */
  constexpr auto to_infnan = [](std::string const& token, auto radix = 10)
  {
    std::unordered_map<std::string, object> static const infnan
    {
      std::make_pair("+inf.0", make<system_float>(+system_float::infinity())),
      std::make_pair("-inf.0", make<system_float>(-system_float::infinity())),
      std::make_pair("+nan.0", make<system_float>(+system_float::quiet_NaN())),
      std::make_pair("-nan.0", make<system_float>(-system_float::quiet_NaN()))
    };

    if (auto iter = infnan.find(token); iter != std::end(infnan))
    {
      return std::get<1>(*iter);
    }
    else
    {
      throw read_error<system_float>("not a number: (string->number ", std::quoted(token), " ", radix, ")");
    }
  };

  /* ---- SRFI-144 -------------------------------------------------------------
   *
   *
   * ------------------------------------------------------------------------ */
  constexpr auto to_constant = [](std::string const& token, auto radix = 10)
  {
    static const std::unordered_map<std::string, object> constants
    {
      std::make_pair("fl-pi", make<system_float>(boost::math::constants::pi<system_float::value_type>())),
    };

    if (auto iter = constants.find(token); iter != std::end(constants))
    {
      return std::get<1>(*iter);
    }
    else
    {
      throw read_error<system_float>("not a number: (string->number ", std::quoted(token), " ", radix, ")");
    }
  };

  /* ---- R7RS 7.1.1 Lexical structure -----------------------------------------
   *
   *  <real R> = <sign> <ureal R> | <infnan>
   *
   *  <sign> = <empty> | + | -
   *
   *  <ureal R> = <uinteger R>
   *            | <uinteger R> / <uinteger R>
   *            | <decimal R>
   *
   * ------------------------------------------------------------------------ */
  constexpr auto to_real = to_integer // <sign> <uinteger R>
                         | to_ratio   // <sign> <uinteger R> / <uinteger R>
                         | to_decimal // <sign> <decimal R>
                         | to_infnan
                         | to_constant; // SRFI-144

  /* ---- R7RS 7.1.1 Lexical structure -----------------------------------------
   *
   *  <complex R> = <real R>
   *              | <real R> @ <real R>                                    TODO
   *              | <real R> + <ureal R> i                                 TODO
   *              | <real R> - <ureal R> i                                 TODO
   *              | <real R> +           i                                 TODO
   *              | <real R> -           i                                 TODO
   *              | <real R>    <infnan> i                                 TODO
   *              |             <infnan> i                                 TODO
   *              |          + <ureal R> i                                 TODO
   *              |          - <ureal R> i                                 TODO
   *              |          +           i                                 TODO
   *              |          -           i                                 TODO
   *
   * ------------------------------------------------------------------------ */
  constexpr auto to_complex = to_real;

  /* ---- R7RS 7.1.1 Lexical structure -----------------------------------------
   *
   *  <number> = <num 2> | <num 8> | <num 10> | <num 16>
   *
   *  <num R> = <prefix R> <complex R>
   *
   *  <prefix R> = <radix R> <exactness> | <exactness> <radix R>
   *
   *  <radix 2>  = #b
   *  <radix 8>  = #o
   *  <radix 10> = #d | <empty>
   *  <radix 16> = #x
   *
   *  <exactness> = <empty> | #i | #e
   *
   *  NOTE: <Prefix R> is parsed with 'read'.
   *
   * ------------------------------------------------------------------------ */
  constexpr auto to_number = to_complex;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_NUMERIC_LITERAL_HPP
