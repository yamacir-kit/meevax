#ifndef INCLUDED_MEEVAX_LISP_ERROR_HPP
#define INCLUDED_MEEVAX_LISP_ERROR_HPP

#ifndef NDEBUG
#define error(...) \
  "(error (file " << __FILE__ << ") (line " << __LINE__ << ") (" << __VA_ARGS__ << "))"
#else
#define error(...) \
  "(error (" << __VA_ARGS__ << "))"
#endif

#endif // INCLUDED_MEEVAX_LISP_ERROR_HPP

