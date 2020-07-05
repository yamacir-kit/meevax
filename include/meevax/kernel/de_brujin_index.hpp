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
    explicit de_bruijn_index(Ts&&... xs)
      : object { lookup(std::forward<decltype(xs)>(xs)...) }
    {}

    const object lookup(const object& value, const object& frames)
    {
      auto layer {0};

      for (const auto& frame : frames)
      {
        auto index {0};

        for (auto iter { std::begin(frame) }; iter != std::end(frame); ++iter)
        {
          if (iter.is<pair>() and compare(*iter, value))
          {
            variadic = false;

            return
              cons(
                make<integer>(layer),
                make<integer>(index));
          }
          else if (iter.is<symbol>() and compare(iter, value))
          {
            variadic = true;

            return
              cons(
                make<integer>(layer),
                make<integer>(index));
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

