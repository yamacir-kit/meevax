#ifndef INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP
#define INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP

#include <numeric> // std::accumulate

#include <meevax/kernel/linker.hpp>
#include <meevax/kernel/list.hpp>

namespace meevax
{
inline namespace kernel
{
  #if __has_cpp_attribute(maybe_unused)
  #define PROCEDURE(...) \
    meevax::object const __VA_ARGS__([[maybe_unused]] meevax::object const& xs)
  #else
  #define PROCEDURE(...) \
    meevax::object const __VA_ARGS__(                 meevax::object const& xs)
  #endif

  struct procedure : public std::function<PROCEDURE()>
  {
    using signature = PROCEDURE((*));

    std::string const name;

    template <typename... Ts>
    explicit procedure(std::string const& name, Ts&&... xs)
      : std::function<PROCEDURE()> { std::forward<decltype(xs)>(xs)...  }
      , name { name }
    {}

    virtual ~procedure() = default;
  };

  auto operator <<(output_port & port, procedure const& datum) -> output_port &;

  template <typename T>
  struct predicate
  {
    let const& operator ()(let const& xs) const
    {
      if (xs.is<null>())
      {
        return f;
      }
      else
      {
        for (let const& x : xs)
        {
          if (x.is<null>() or not x.is<T>())
          {
            return f;
          }
        }

        return t;
      }
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP
