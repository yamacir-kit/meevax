#ifndef INCLUDED_MEEVAX_UTILITY_BINDER_HPP
#define INCLUDED_MEEVAX_UTILITY_BINDER_HPP

#include <typeinfo>
#include <utility>

#include <meevax/facade/conditionally_trivial_destructible.hpp>

// TODO
// デフォルト引数用プレースホルダクラスと基底クラスの追加
// 基底クラス作成の簡易化のためのファサードクラスの追加

namespace meevax::utility
{
  template <typename T, typename Base>
  struct binder
    // : public facade::conditionally_trivial_destructible<T>,
    : public T,
      public Base
  {
    // using bound = facade::conditionally_trivial_destructible<T>;
    using bound = T;

    explicit constexpr binder(const T& value)
      : bound {value}
    {}

    template <typename... Ts>
    explicit constexpr binder(Ts&&... xs)
      : bound {std::forward<Ts>(xs)...}
    {}

    auto type() const noexcept
      -> const std::type_info& override
    {
      return typeid(T);
    }
  };
} // namespace meevex::utility

#endif // INCLUDED_MEEVAX_UTILITY_BINDER_HPP

