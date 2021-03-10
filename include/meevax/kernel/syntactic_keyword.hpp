#ifndef INCLUDED_MEEVAX_KERNEL_SYNTACTIC_KEYWORD_HPP
#define INCLUDED_MEEVAX_KERNEL_SYNTACTIC_KEYWORD_HPP

#include <meevax/kernel/list.hpp>
#include <meevax/kernel/symbol.hpp>

namespace meevax
{
inline namespace kernel
{
  struct syntactic_keyword : public virtual pair
  {
    using pair::pair;

    decltype(auto) assq() const
    {
      return kernel::assq(car(*this), cdr(*this));
    }

    let const& lookup() const
    {
      if (let const& x = assq(); x != f)
      {
        return cdr(x);
      }
      else
      {
        return car(*this);
      }
    }

    auto is_free() const
    {
      return assq().eqv(f);
    }

    auto is_bound() const
    {
      return not is_free();
    }

    friend auto operator <<(output_port & port, syntactic_keyword const& datum) -> output_port &
    {
      return port << underline << car(datum) << reset;
    }
  };

  auto lookup(let const& x, let const& env)
  {
    if (let const& p = assq(x, env); not p.eqv(f))
    {
      return cdr(p);
    }
    else
    {
      return x.is<syntactic_keyword>() ? x.as<syntactic_keyword>().lookup() : x;
    }
  }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_SYNTACTIC_KEYWORD_HPP
