#ifndef INCLUDED_MEEVAX_KERNEL_IDENTIFIER_HPP
#define INCLUDED_MEEVAX_KERNEL_IDENTIFIER_HPP

#include <meevax/kernel/syntactic_closure.hpp>

namespace meevax { inline namespace kernel
{
  /* ==== Identifier? ==========================================================
   *
   * (define (identifier? x)
   *   (or (symbol? x)
   *       (and (syntactic-closure? x)
   *            (symbol? (car x)))))
   *
   * ======================================================================== */
  auto is_identifier(let const & x)
  {
    if (x.is<null>())
    {
      return false;
    }
    else if (x.is<syntactic_closure>())
    {
      return x.as<syntactic_closure>().is_identifier();
    }
    else
    {
      return x.is<symbol>();
    }
  }

  auto strip(let const & id)
  {
    if (not id.is<null>() and id.is<syntactic_closure>())
    {
      return id.as<syntactic_closure>().strip();
    }
    else
    {
      return id;
    }
  }

  auto lookup(let const& x, let const& env)
  {
    if (let const& binding = assq(x, env); not binding.eqv(f))
    {
      return cadr(binding); // TODO must be cdr(binding)
    }
    else
    {
      return strip(x);
    }
  }
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_IDENTIFIER_HPP
