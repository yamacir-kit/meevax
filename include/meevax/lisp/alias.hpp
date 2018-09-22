#ifndef INCLUDED_MEEVAX_LISP_ALIAS_HPP
#define INCLUDED_MEEVAX_LISP_ALIAS_HPP

#include <functional>
#include <memory>
#include <string>

namespace meevax::lisp
{
  class cell;

  using cref = const std::shared_ptr<cell>;

  using special = std::function<cref (cref&, cref&)>;
  using symbol = std::string;
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_ALIAS_HPP

