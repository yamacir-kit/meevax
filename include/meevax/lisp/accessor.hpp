#ifndef INCLUDED_MEEVAX_LISP_ACCESSOR_HPP
#define INCLUDED_MEEVAX_LISP_ACCESSOR_HPP

#include <utility>

#include <meevax/lisp/cell.hpp>

#define caar(...) car<0, 0>(__VA_ARGS__)
#define cadar(...) car<0, 1>(__VA_ARGS__)
#define caddar(...) car<0, 2>(__VA_ARGS__)

#define cadr(...) car<1>(__VA_ARGS__)
#define caddr(...) car<2>(__VA_ARGS__)
#define cadddr(...) car<3>(__VA_ARGS__)

namespace meevax::lisp
{
  template <auto N, typename T>
  decltype(auto) cdr(T&& e)
  {
    if constexpr (N)
    {
      return cdr<N-1>(cdr(e));
    }
    else
    {
      return e;
    }
  }

  template <auto N, auto... Ns, typename T>
  decltype(auto) car(T&& e)
  {
    if constexpr (sizeof...(Ns))
    {
      return car<Ns...>(car(cdr<N>(e)));
    }
    else
    {
      return car(cdr<N>(e));
    }
  }
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_ACCESSOR_HPP


