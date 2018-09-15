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
  template <auto N, auto... Ns>
  decltype(auto) car(const std::shared_ptr<cell>& e)
  {
    auto cursor {e};

    for (auto n {0}; n < N; ++n)
    {
      cursor = cdr(cursor);
    }

    if constexpr (sizeof...(Ns) != 0)
    {
      return car<Ns...>(car(cursor));
    }
    else
    {
      return car(cursor);
    }
  }
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_ACCESSOR_HPP


