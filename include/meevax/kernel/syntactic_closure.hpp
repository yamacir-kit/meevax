#ifndef INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CLOSURE_HPP
#define INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CLOSURE_HPP

#include <meevax/kernel/list.hpp>
#include <meevax/kernel/pair.hpp>
#include <meevax/kernel/symbol.hpp>

namespace meevax::kernel
{
  struct syntactic_closure
    : public virtual pair
  {
    using pair::pair; // Inheriting Constructors

    auto symbol() const noexcept -> const auto&
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
        if (car(each) == symbol())
        {
          return cadr(each);
        }
      }

      return symbol();
    }

    auto is_free() const
    {
      return assq(symbol(), syntactic_environment()).eqv(f);
    }

    friend auto operator <<(std::ostream& os, const syntactic_closure& sc)
      -> decltype(os)
    {
      return os << console::underline << sc.symbol() << "." << &sc << console::reset;
    }
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CLOSURE_HPP
