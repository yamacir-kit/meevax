#ifndef INCLUDED_MEEVAX_LISP_ALIAS_HPP
#define INCLUDED_MEEVAX_LISP_ALIAS_HPP

#include <functional>
#include <memory>
#include <string>

namespace meevax::lisp
{
  class cell;
  class cursor;

  using special = const std::function<cursor (cursor, cursor)>;
  using symbol = const std::string;
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_ALIAS_HPP

