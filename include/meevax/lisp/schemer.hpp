#ifndef INCLUDED_MEEVAX_LISP_SCHEMER_HPP
#define INCLUDED_MEEVAX_LISP_SCHEMER_HPP

#include <iostream>
#include <string>

#include <meevax/lisp/cell.hpp>
#include <meevax/lisp/reader.hpp>
#include <meevax/lisp/table.hpp>
#include <meevax/posix/noncanonical.hpp>

// 第一目標：ASTがインクリメンタルに構築できる

namespace meevax::lisp
{
  class schemer
  {
    cursor data;

  public:
    schemer()
      : data {symbols.intern("nil")}
    {}

    void operator()()
    {
      for (std::string buffer {}; true; ) try
      {
        // 描画（ゆくゆくはASTの書き出しのみに置き換えられるべき）
        std::cout << ">> " << (buffer += std::getchar()) << std::endl;

        // ASTの構築（不正な場合は例外を投げて継続）
        const auto well_formed_expression {read(buffer)};

        std::cout << "\n=> " << eval(well_formed_expression) << "\n\n";
        buffer.clear();
      }
      catch (const std::string&) // unbalance expression
      {
      }
    }
  };
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_SCHEMER_HPP

