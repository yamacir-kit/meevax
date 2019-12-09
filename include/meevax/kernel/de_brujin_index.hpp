#ifndef INCLUDED_MEEVAX_KERNEL_DE_BRUJIN_INDEX_HPP
#define INCLUDED_MEEVAX_KERNEL_DE_BRUJIN_INDEX_HPP

#include <meevax/kernel/list.hpp>

namespace meevax::kernel
{
  template <typename Comparator = default_equivalence_comparator>
  class de_bruijn_index
    : public object
  {
    bool variadic;

  public:
    Comparator compare {};

    template <typename... Ts>
    explicit de_bruijn_index(Ts&&... operands)
      : object {lookup(std::forward<decltype(operands)>(operands)...)}
    {}

    const object
      lookup(
        const object& value,
        const object& frames)
    {
      auto layer {0};

      for (const auto& frame : frames)
      {
        auto index {0};

        for (homoiconic_iterator node {frame}; node; ++node)
        {
          if (node.is<pair>() and compare(*node, value))
          {
            variadic = false;

            return
              cons(
                make<real>(layer),
                make<real>(index));
          }
          else if (node.is<symbol>() and compare(node, value))
          {
            variadic = true;

            return
              cons(
                make<real>(layer),
                make<real>(index));
          }

          ++index;
        }

        ++layer;
      }

      return unit;
    }

    bool is_variadic() const noexcept
    {
      return variadic;
    }
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_DE_BRUJIN_INDEX_HPP

