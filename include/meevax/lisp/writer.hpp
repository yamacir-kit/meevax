#ifndef INCLUDED_MEEVAX_LISP_WRITER_HPP
#define INCLUDED_MEEVAX_LISP_WRITER_HPP

#include <iostream>
#include <sstream>
#include <typeinfo>

#include <meevax/lisp/closure.hpp>
#include <meevax/lisp/iterator.hpp>
#include <meevax/lisp/pair.hpp>

namespace meevax::lisp
{
  std::ostream& operator<<(std::ostream& os, const cursor& exp)
  {
    if (!exp)
    {
      return os << "nil";
    }

    if (exp->type() == typeid(std::string))
    {
      return os << exp->as<std::string>();
    }
    else if (exp->type() == typeid(closure))
    {
      return os << "<closure>";
    }

    os << "(" << car(exp);

    for (auto iter {cdr(exp)}; iter; ++iter)
    {
      if (iter->type() != typeid(pair))
      {
        return os << " . " << iter << ")";
      }
      else
      {
        os << " " << car(iter);
      }
    }

    // return os << "(" << car(e) << " . " << cdr(e) << ")";
    return os << ")";
  }

  decltype(auto) to_string(const cursor& exp)
  {
    std::ostringstream ss {};
    ss << exp;
    return ss.str();
  }
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_WRITER_HPP

