#ifndef INCLUDED_MEEVAX_LISP_WRITER_HPP
#define INCLUDED_MEEVAX_LISP_WRITER_HPP

#include <iostream>
#include <sstream>

#include <meevax/lisp/cell.hpp>

namespace meevax::lisp
{
  // TODO command line parameter
  bool abbreviate {true};

  std::ostream& operator<<(std::ostream& os, cursor e)
  {
    if (!e)
    {
      return os << "nil";
    }

    if (e->type() == typeid(std::string))
    {
      return os << e->as<std::string>();
    }

    // XXX DIRTY HACK
    if (e->type() != typeid(cell))
    {
      return os << "<closure>";
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

  auto to_string(const cursor& cursor)
  {
    std::ostringstream ss {};
    ss << cursor;
    return ss.str();
  }
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_WRITER_HPP

