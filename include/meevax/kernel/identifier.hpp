#ifndef INCLUDED_MEEVAX_KERNEL_IDENTIFIER_HPP
#define INCLUDED_MEEVAX_KERNEL_IDENTIFIER_HPP

#include <meevax/kernel/symbol.hpp>
#include <meevax/kernel/syntactic_closure.hpp>

namespace meevax::kernel
{
  /* ==== Identifier? ==========================================================
   *
   * (define (identifier? x)
   *   (or (symbol? x)
   *       (and (syntactic-closure? x)
   *            (symbol? (car x)))))
   *
   * ======================================================================= */
  auto is_identifier(const object& x)
  {
    if (not null(x))
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

  auto strip(const object& id)
  {
    if (not null(id) and id.is<syntactic_closure>())
    {
      return id.as<syntactic_closure>().strip();
    }
    else
    {
      return id;
    }
  }

  auto lookup(const object& x, const object& env)
  {
    if (const object binding { assq(x, env) }; not binding.eqv(f))
    {
      return cadr(binding); // TODO must be cdr(binding)
    }
    else
    {
      return strip(x);
    }
  }
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_IDENTIFIER_HPP

