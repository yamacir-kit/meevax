#ifndef INCLUDED_MEEVAX_KERNEL_NUMERIC_LITERAL_HPP
#define INCLUDED_MEEVAX_KERNEL_NUMERIC_LITERAL_HPP

#include <meevax/kernel/number.hpp>

namespace meevax
{
inline namespace kernel
{
  /* ---- R7RS 7.1.1 Lexical structure -----------------------------------------
   *
   *  <number> = <num 2> | <num 8> | <num 10> | <num 16>
   *
   *  <num R> = <prefix R> <complex R>
   *
   *  NOTE: <Prefix R> is parsed with 'read'.
   *
   *  <complex R> = <real R>
   *              | <real R> @ <real R>
   *              | <real R> + <ureal R> i
   *              | <real R> - <ureal R> i
   *              | <real R> + i
   *              | <real R> - i
   *              | <real R> <infnan> i
   *              | + <ureal R> i
   *              | - <ureal R> i
   *              | <infnan> i
   *              | + i
   *              | - i
   *
   *  <real R> = <sign> <ureal R> | <infnan>
   *
   *  <ureal R> = <uinteger R>
   *            | <uinteger R> / <uinteger R>
   *            | <decimal R>
   *
   *  <decimal 10> = <uinteger 10> <suffix>
   *               | . <digit 10>+ <suffix>
   *               | <digit 10>+ . <digit 10>* <suffix>
   *
   *  <uinteger R> = <digit R>+
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
   *  <infnan> = +inf.0 | -inf.0 | +nan.0 | -nan.0
   *
   *  <suffix> = <empty> | <exponent marker> <sign> <digit 10>+
   *
   *  <exponent marker> = e
   *
   *  <sign> = <empty> | + | -
   *
   *  <digit 2>  = 0 | 1
   *  <digit 8>  = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7
   *  <digit 10> = <digit>
   *  <digit 16> = <digit 10> | a | b | c | d | e | f
   *
   * ------------------------------------------------------------------------ */

  // bytestring const exactness = "(#e|#i)?";
  //
  // template <auto R>
  // bytestring radix = "";
  //
  // template <> bytestring radix<2>  = "(#b)";
  // template <> bytestring radix<8>  = "(#o)";
  // template <> bytestring radix<10> = "(#d)?";
  // template <> bytestring radix<16> = "(#x)";
  //
  // template <auto R>
  // auto const prefix = "(?:" + radix<R> + exactness + "|" + exactness + radix<R> + ")";
  //
  // template <auto R>
  // let make_num(bytestring const& s)
  // {
  // }

  /* ---------------------------------------------------------------------------
   *
   *  <number> = <num 2> | <num 8> | <num 10> | <num 16>
   *
   * ------------------------------------------------------------------------ */
  // template <auto R = 10>
  // let make_number(bytestring const& s)
  // {
  //   return make_num<R>(s);
  // }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_NUMERIC_LITERAL_HPP
