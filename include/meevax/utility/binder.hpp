#ifndef INCLUDED_MEEVAX_UTILITY_BINDER_HPP
#define INCLUDED_MEEVAX_UTILITY_BINDER_HPP

#include <typeinfo>
#include <utility>

// TODO
// デフォルト引数用プレースホルダクラスと基底クラスの追加
// 基底クラス作成の簡易化のためのファサードクラスの追加

namespace meevax::utility
{
  template <typename T, typename Base>
  struct binder
    : public T,
      public Base
  {
    template <typename... Ts>
    explicit constexpr binder(Ts&&... xs)
      : T {std::forward<Ts>(xs)...}
    {}

    auto type() const noexcept
      -> const std::type_info& override
    {
      return typeid(T);
    }
  };
} // namespace meevex::utility

#endif // INCLUDED_MEEVAX_UTILITY_BINDER_HPP

