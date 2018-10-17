#ifndef INCLUDED_MEEVAX_LISP_ERROR_HPP
#define INCLUDED_MEEVAX_LISP_ERROR_HPP

#define error(...) \
  "\e[32m(\e[1;31merror \e[32m(file \e[36m" << __FILE__ << "\e[32m) (line \e[36m" << __LINE__ << ") (" << __VA_ARGS__ << ")\e[32m)\e[0m"

#endif // INCLUDED_MEEVAX_LISP_ERROR_HPP

