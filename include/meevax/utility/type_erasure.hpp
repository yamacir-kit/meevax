#ifndef INCLUDED_MEEVAX_UTILITY_TYPE_ERASURE_HPP
#define INCLUDED_MEEVAX_UTILITY_TYPE_ERASURE_HPP

#include <type_traits>
#include <typeinfo>
#include <utility>

namespace meevax::utility
{
  template <typename T, typename B>
  struct binder
    : public T,
      public virtual B
  {
    template <typename... Ts>
    explicit constexpr binder(Ts&&... args)
      : std::conditional<std::is_base_of<B, T>::value, B, T>::type {std::forward<Ts>(args)...}
    {}

    auto type() const noexcept
      -> const std::type_info& override
    {
      return typeid(T);
    }
  };
} // namespace meevex::utility

#endif // INCLUDED_MEEVAX_UTILITY_TYPE_ERASURE_HPP

