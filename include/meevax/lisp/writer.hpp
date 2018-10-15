#ifndef INCLUDED_MEEVAX_LISP_WRITER_HPP
#define INCLUDED_MEEVAX_LISP_WRITER_HPP

#include <iostream>

#include <meevax/lisp/cell.hpp>

namespace meevax::lisp
{
  bool abbreviate {true};

  std::ostream& operator<<(std::ostream& os, cursor e)
  {
    if (!e)
    {
      return os << "nil";
    }

    if (e->type() == typeid(std::string))
    {
      return os << e->template as<std::string>();
    }

    if (abbreviate)
    {
      for (os << "(" << *e; ++e; os << " " << *e)
      {
        if (e->type() != typeid(cell))
        {
          return os << " . " << e << ")";
        }
      }
    }
    else
    {
      return os << "(" << *e << " . " << ++e << ")";
    }

    return os << ")";
  }
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_WRITER_HPP

