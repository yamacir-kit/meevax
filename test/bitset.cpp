#include <bitset>
#include <cassert>
#include <chrono>
#include <functional>
#include <iostream>
#include <map>

#undef NDEBUG

#include <meevax/utility/debug.hpp>
#include <meevax/iterator/index_iterator.hpp>

using period = std::chrono::nanoseconds;

template <typename Period = period, typename Thunk>
auto duration(Thunk thunk)
{
  auto begin = std::chrono::high_resolution_clock::now();
  thunk();
  return std::chrono::duration_cast<Period>(std::chrono::high_resolution_clock::now() - begin);
}

namespace std
{
  template <auto N>
  auto begin(std::bitset<N> const& bitset)
  {
    return meevax::index_iterator(bitset, 0);
  }

  template <auto N>
  auto end(std::bitset<N> const& bitset)
  {
    return meevax::index_iterator(bitset, bitset.size());
  }
}

template <typename Bitset, typename... Ts>
auto test(Ts&&... xs)
{
  auto result = std::map<std::string, period>();

  auto bitset = Bitset(std::forward<decltype(xs)>(xs)...);

  result.emplace("set", duration([&]()
  {
    for (std::size_t i = 0; i < bitset.size(); ++i)
    {
      bitset.set(i);
    }
  }));

  result.emplace("iterate", duration([&]()
  {
    for ([[maybe_unused]] auto bit : bitset)
    {
      assert(bit);
    }
  }));

  result.emplace("count", duration([&]()
  {
    assert(std::count_if(std::begin(bitset), std::end(bitset), [](auto&& each) { return each; }) == bitset.size());
  }));

  result.emplace("test", duration([&]()
  {
    for (std::size_t i = 0; i < bitset.size(); ++i)
    {
      assert(bitset.test(i));
    }
  }));

  return result;
}

auto main() -> int
{
  constexpr std::size_t size = 1024 * 1024;

  const auto result_std_bitset = test<std::bitset<size>>();

  for (auto&& [title, duration] : result_std_bitset)
  {
    std::cout << title << ": " << duration.count() << std::endl;
  }

  return EXIT_SUCCESS;
}
