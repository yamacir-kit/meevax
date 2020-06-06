#ifndef INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CLOSURE_HPP
#define INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CLOSURE_HPP

#include <meevax/kernel/pair.hpp>
#include <meevax/kernel/symbol.hpp>

namespace meevax::kernel
{
  struct syntactic_closure
    : public virtual pair
  {
    using pair::pair; // Inheriting Constructors

    auto syntax_quote() const noexcept -> const auto&
    {
      return first;
    }

    auto syntactic_environment() const noexcept -> const auto&
    {
      return second;
    }

    auto lookup() const -> const auto&
    {
      for (const auto& each : syntactic_environment())
      {
        if (car(each) == syntax_quote())
        {
          return cadr(each);
        }
      }

      return syntax_quote();
    }

    auto is_keyword() const
    {
      return lookup() == syntax_quote();
    }

    friend auto operator <<(std::ostream& os, const syntactic_closure& sc)
      -> decltype(os)
    {
      return os << console::underline << sc.syntax_quote() << "." << &sc << console::reset;
    }
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CLOSURE_HPP
