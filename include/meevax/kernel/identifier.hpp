#ifndef INCLUDED_MEEVAX_KERNEL_IDENTIFIER_HPP
#define INCLUDED_MEEVAX_KERNEL_IDENTIFIER_HPP

#include <meevax/kernel/syntactic_keyword.hpp>

namespace meevax
{
inline namespace kernel
{
  /* ---- Identifier? ----------------------------------------------------------
   *
   *  (define (identifier? x)
   *    (or (symbol? x)
   *        (and (syntactic-keyword? x)
   *             (symbol? (car x)))))
   *
   * ------------------------------------------------------------------------ */
  // [[deprecated]]
  // auto is_identifier(let const& x)
  // {
  //   return x.is<syntactic_keyword>() ? x.as<syntactic_keyword>().is_identifier() : x.is<symbol>();
  // }

  auto strip(let const& x)
  {
    return x.is<syntactic_keyword>() ? x.as<syntactic_keyword>().strip() : x;
  }

  auto lookup(let const& x, let const& env)
  {
    if (let const& binding = assq(x, env); not binding.eqv(f))
    {
      return cdr(binding);
    }
    else
    {
      return strip(x);
    }
  }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_IDENTIFIER_HPP
