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

    auto lookup() const
    {
      return assq(car(*this), cdr(*this));
    }

    let const& unwrap() const
    {
      if (let const& x = lookup(); x != f)
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
      return lookup().eqv(f);
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
    if (let const& binding = assq(x, env); not binding.eqv(f))
    {
      return cdr(binding);
    }
    else
    {
      return x.is<syntactic_keyword>() ? x.as<syntactic_keyword>().unwrap() : x;
    }
  }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_SYNTACTIC_KEYWORD_HPP
