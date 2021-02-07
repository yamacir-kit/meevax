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
            REQUIRES(std::is_invocable<F, bytestring const&, int>),
            REQUIRES(std::is_invocable<G, bytestring const&, int>)>
  auto operator |(F&& f, G&& g)
  {
    return [=](bytestring const& token, auto radix = 10)
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
  auto make_integer = [](bytestring const& token, auto radix = 10)
  {
    std::regex static const pattern { "[+-]?[\\dabcdef]+" };

    if (std::smatch result; std::regex_match(token, result, pattern))
    {
      switch (radix)
      {
      case  2: return make<exact_integer>("0b" + token.substr(token[0] == '+' ? 1 : 0));
      case  8: return make<exact_integer>("0"  + token.substr(token[0] == '+' ? 1 : 0));
      case 10: return make<exact_integer>(       token.substr(token[0] == '+' ? 1 : 0));
      case 16: return make<exact_integer>("0x" + token.substr(token[0] == '+' ? 1 : 0));

      default:
        throw read_error<exact_integer>(token, " is not an integer's external representation");
      }
    }
    else
    {
      throw read_error<exact_integer>(token, " is not an integer's external representation");
    }
  };

  auto make_ratio = [](bytestring const& token, auto radix = 10)
  {
    std::regex static const pattern { "([+-]?[\\dabcdef]+)/([\\dabcdef]+)" };

    if (std::smatch result; std::regex_match(token, result, pattern))
    {
      if (auto const value =
            ratio(make_integer(result.str(1), radix),
                  make_integer(result.str(2), radix)).reduce(); value.is_integer())
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
      throw read_error<void>(__FILE__, ":", __LINE__);
    }
  };

  /* ---- R7RS 7.1.1 Lexical structure -----------------------------------------
   *
   *  <infnan> = +inf.0 | -inf.0 | +nan.0 | -nan.0
   *
   * ------------------------------------------------------------------------ */
  auto make_infnan = [](bytestring const& token, auto)
  {
    std::unordered_map<bytestring, object> static const infnan
    {
      std::make_pair("+inf.0", make<default_float>(+default_float::infinity())),
      std::make_pair("-inf.0", make<default_float>(-default_float::infinity())),
      std::make_pair("+nan.0", make<default_float>(+default_float::quiet_NaN())),
      std::make_pair("-nan.0", make<default_float>(-default_float::quiet_NaN()))
    };

    return infnan.at(token);
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
  auto make_decimal = [](bytestring const& token, auto radix = 10) // <sign> <decimal 10>
  {
    std::regex static const p2 { "[+-]?.\\d+" };
    std::regex static const p3 { "[+-]?\\d+.\\d*" };

    if (std::smatch result; std::regex_match(token, result, p2) or
                            std::regex_match(token, result, p3))
    {
      switch (radix)
      {
      case 10:
        return make<default_float>(token.substr(token[0] == '+' ? 1 : 0));

      default:
        throw read_error<void>(
          "There are no rules for <decimal 2>, <decimal 8>, and <decimal 16>, "
          "which means that numbers containing decimal points or exponents are "
          "always in decimal radix.");
      }
    }
    else
    {
      throw read_error<exact_integer>(token, " is not a decimal's external representation");
    }
  };

  auto make_srfi_144 = [](bytestring const& token, auto = 10)
  {
    static const std::unordered_map<bytestring, object> srfi_144
    {
      std::make_pair("fl-pi", make<default_float>(boost::math::constants::pi<default_float::value_type>())),
    };

    return srfi_144.at(token);
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
  auto make_real = make_integer // <sign> <uinteger R>
                 | make_ratio // <sign> <uinteger R> / <uinteger R>
                 | make_decimal // <sign> <decimal R>
                 | make_infnan
                 | make_srfi_144;

  /* ---- R7RS 7.1.1 Lexical structure -----------------------------------------
   *
   *  <complex R> = <real R>
   *              | <real R> @ <real R>                                    TODO
   *              | <real R> + <ureal R> i                                 TODO
   *              | <real R> - <ureal R> i                                 TODO
   *              | <real R> + i                                           TODO
   *              | <real R> - i                                           TODO
   *              | <real R> <infnan> i                                    TODO
   *              | + <ureal R> i                                          TODO
   *              | - <ureal R> i                                          TODO
   *              | <infnan> i                                             TODO
   *              | + i                                                    TODO
   *              | - i                                                    TODO
   *
   * ------------------------------------------------------------------------ */
  auto make_complex = make_real;

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
  auto make_number = make_complex;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_NUMERIC_LITERAL_HPP
