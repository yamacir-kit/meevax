#ifndef INCLUDED_MEEVAX_ALGORITHM_FOLD_HPP
#define INCLUDED_MEEVAX_ALGORITHM_FOLD_HPP

#include <iterator>
#include <numeric>
#include <utility>

namespace meevax::algorithm
{
  template <typename... Ts>
  decltype(auto) fold(Ts&&... args)
  {
    return std::accumulate(std::forward<Ts>(args)...);
  }

  template <typename InputIterator, typename T, typename BinaryOperation>
  decltype(auto) fold_right(InputIterator&& begin,
                            InputIterator&& end,
                            T&& init,
                            BinaryOperation&& operation)
  {
    return std::accumulate(
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
} // namespace meevax::algorithm

#endif // INCLUDED_MEEVAX_ALGORITHM_FOLD_HPP

