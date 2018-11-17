#ifndef INCLUDED_MEEVAX_TUPLE_ACCESSOR_HPP
#define INCLUDED_MEEVAX_TUPLE_ACCESSOR_HPP

#include <memory>
#include <tuple>
#include <type_traits>
#include <utility>

namespace meevax::tuple
{
  template <auto N>
  struct accessor
    : public std::integral_constant<std::size_t, N>
  {
    template <typename... Ts>
    decltype(auto) operator()(const std::tuple<Ts...>& tuple)
    {
      return std::get<N>(tuple);
    }

    // XXX DIRTY HACK!!!
    template <typename T>
    decltype(auto) operator()(const std::shared_ptr<T>& pointer)
    {
      return std::get<N>(pointer.std::shared_ptr<T>::operator*());
    }
  };
} // namespace meevax::tuple

#endif // INCLUDED_MEEVAX_TUPLE_ACCESSOR_HPP

