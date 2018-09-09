#ifndef INCLUDED_MEEVAX_LISP_ERROR_HPP
#define INCLUDED_MEEVAX_LISP_ERROR_HPP

#define error(...) \
  "\e[32m(\e[1;31merror \e[32m(file " << __FILE__ << ") (line " << __LINE__ << ") (" << __VA_ARGS__ << "))\e[0m"

#endif // INCLUDED_MEEVAX_LISP_ERROR_HPP

