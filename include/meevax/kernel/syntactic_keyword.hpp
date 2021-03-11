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

    let const& unwrap_syntax() const noexcept
    {
      return first;
    }

    let const& global_environment() const noexcept
    {
      return second;
    }

    decltype(auto) assq() const
    {
      return kernel::assq(unwrap_syntax(), global_environment());
    }

    let const& lookup() const
    {
      if (let const& x = assq(); x != f)
      {
        return cdr(x);
      }
      else
      {
        return unwrap_syntax();
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
      return port << underline << datum.unwrap_syntax() << reset;
    }
  };

  auto lookup(let const& x, let const& g)
  {
    if (let const& p = assq(x, g); p != f)
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
