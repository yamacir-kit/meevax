#ifndef INCLUDED_MEEVAX_LISP_CLOSURE_HPP
#define INCLUDED_MEEVAX_LISP_CLOSURE_HPP

#include <tuple>

#include <meevax/lisp/cell.hpp>

namespace meevax::lisp
{
  // クロージャは関数とそれ自身が定義された環境のペア
  struct closure
    : public virtual cell
  {
    template <typename... Ts>
    explicit constexpr closure(Ts&&... args)
    {
      // バインダのメンバイニシャライザリストからコイツが呼ばれる前に
      // 仮想基底クラスであるセルのデフォルトコンストラクタが走ってしまう
      cell::operator=(std::forward_as_tuple(args...));
    }
  };
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_CLOSURE_HPP

