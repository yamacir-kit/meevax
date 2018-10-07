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
      for (std::string buffer {}; std::cout << buffer; ) try
      {
        buffer.push_back(std::getchar());

        const auto built {read(buffer)};
        const auto result {eval(built)};

        std::cout << "\n\n" << result << "\n\n";

        buffer.clear();
      }
      catch (const std::string&)
      {
        std::cout << "\n";
      }
    }
  };
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_SCHEMER_HPP

