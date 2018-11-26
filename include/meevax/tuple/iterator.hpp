#ifndef INCLUDED_MEEVAX_TUPLE_ITERATOR_HPP
#define INCLUDED_MEEVAX_TUPLE_ITERATOR_HPP

#include <iterator>

namespace meevax::tuple
{
  template <typename SmartPointer, auto N = 1>
  struct [[deprecated]] iterator
    : public SmartPointer,
      public std::iterator<std::input_iterator_tag, typename SmartPointer::element_type>
  {
    template <typename... Ts>
    constexpr iterator(Ts&&... args)
      : SmartPointer {std::forward<Ts>(args)...}
    {}

    decltype(auto) operator*() const
    {
      return std::get<0>(SmartPointer::operator*());
    }

    decltype(auto) operator++()
    {
      return *this = std::get<N>(SmartPointer::operator*());
    }
  };
} // namespace meevax::tuple

#endif // INCLUDED_MEEVAX_TUPLE_ITERATOR_HPP

