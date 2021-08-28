/*
   Copyright 2018-2021 Tatsuya Yamasaki.

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

#ifndef INCLUDED_MEEVAX_KERNEL_NUMERIC_LITERAL_HPP
#define INCLUDED_MEEVAX_KERNEL_NUMERIC_LITERAL_HPP

#include <regex>

#include <meevax/kernel/constant.hpp>

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

  auto to_integer = [](std::string const& token, auto radix = 10)
  {
    auto result = exact_integer(token, radix);
    return make(result);
  };

  auto to_ratio = [](std::string const& token, auto radix = 10)
  {
    if (auto const value = ratio(token, radix).reduce(); value.is_integer())
    {
      return std::get<0>(value);
    }
    else
    {
      return make(value);
    }
  };

  auto to_decimal = [](std::string const& token, auto)
  {
    auto result = system_float(token);
    return make(result);
  };

  /* ---- R7RS 7.1.1 Lexical structure -----------------------------------------
   *
   *  <infnan> = +inf.0 | -inf.0 | +nan.0 | -nan.0
   *
   *  <srfi 144> = fl-pi                                                   TODO
   *
   * ------------------------------------------------------------------------ */
  auto to_constant = [](std::string const& token, auto = 10)
  {
    if (auto iter = constants.find(token); iter != std::end(constants))
    {
      return std::get<1>(*iter);
    }
    else
    {
      throw read_error(make<string>("not a number"), make<string>(token));
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
  auto to_real = to_integer // <sign> <uinteger R>
               | to_ratio   // <sign> <uinteger R> / <uinteger R>
               | to_decimal // <sign> <decimal R>
               | to_constant; // <infnan> or SRFI-144

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
  auto to_complex = to_real;

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
  auto to_number = to_complex;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_NUMERIC_LITERAL_HPP
