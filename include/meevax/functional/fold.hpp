#ifndef INCLUDED_MEEVAX_FUNCTIONAL_FOLD_HPP
#define INCLUDED_MEEVAX_FUNCTIONAL_FOLD_HPP

#include <iterator>
#include <numeric>
#include <utility>

namespace meevax::functional
{
  template <typename... Ts>
  decltype(auto) foldl(Ts&&... args)
  {
    return std::accumulate(std::forward<Ts>(args)...);
  }

  template <typename InputIterator, typename T, typename BinaryOperation>
  decltype(auto) foldr(InputIterator&& begin, InputIterator&& end, T&& init, BinaryOperation operation = std::plus<T> {})
  {
    return foldl(
             std::reverse_iterator {std::forward<InputIterator>(end)},
             std::reverse_iterator {std::forward<InputIterator>(begin)},
             std::forward<T>(init),
             [&](auto&& lhs, auto&& rhs)
             {
               return operation(
                        std::forward<decltype(rhs)>(rhs),
                        std::forward<decltype(lhs)>(lhs)
                      );
             }
           );
  }

  template <typename SequenceContainer, typename T, typename BinaryOperation>
  decltype(auto) foldr(const SequenceContainer& container, T&& init, BinaryOperation&& operation = std::plus<T> {})
  {
    return foldr(
             std::begin(container),
             std::end(container),
             std::forward<T>(init),
             std::forward<BinaryOperation>(operation)
           );
  }
} // namespace meevax::functional

#endif // INCLUDED_MEEVAX_FUNCTIONAL_FOLD_HPP

