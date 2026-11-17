#include <algorithm>
#include <array>
#include <chrono>
#include <concepts>
#include <cstdint>
#include <iomanip>
#include <iostream>
#include <random>
#include <ranges>
#include <set>
#include <vector>

#include <meevax/memory/pointer_set.hpp>
#include <meevax/memory/multichannel_pointer_set.hpp>

auto duration(auto thunk)
{
  auto const begin = std::chrono::steady_clock::now();
  thunk();
  return std::chrono::duration<double, std::milli>(std::chrono::steady_clock::now() - begin).count();
}

template <typename Set, typename Element>
auto measure(std::vector<Element const*> const& pointers)
{
  auto set = Set();

  auto checksum = std::uintptr_t(0);

  std::cout << '\t' << duration([&]()
  {
    for (auto p : pointers)
    {
      set.insert(p);
    }
  });

  std::cout << '\t' << duration([&]()
  {
    for (auto p : pointers)
    {
      checksum += set.contains(p);
    }
  });

  if constexpr (requires { set.lower_bound(pointers.front()); })
  {
    std::cout << '\t' << duration([&]()
    {
      for (auto p : pointers)
      {
        checksum += reinterpret_cast<std::uintptr_t>(*set.lower_bound(p));
      }
    });
  }
  else
  {
    std::cout << "\t-";
  }

  std::cout << '\t' << duration([&]()
  {
    for (auto p : set)
    {
      checksum += reinterpret_cast<std::uintptr_t>(p);
    }
  });

  if constexpr (std::same_as<decltype(set.begin()), decltype(set.end())> and requires (decltype(set.begin()) i) { --i; })
  {
    std::cout << '\t' << duration([&]()
    {
      auto const first = set.begin();

      for (auto i = set.end(); i != first; )
      {
        checksum += reinterpret_cast<std::uintptr_t>(*--i);
      }
    });
  }
  else
  {
    std::cout << "\t-";
  }

  if constexpr (requires { set.erase(pointers.front()); })
  {
    std::cout << '\t' << duration([&]()
    {
      for (auto p : pointers)
      {
        set.erase(p);
      }
    });
  }
  else
  {
    std::cout << "\t-";
  }

  std::cout << '\t' << checksum << '\n';

  set.clear();
}

template <auto N>
auto compare()
{
  using element = std::array<std::uint64_t, N>;

  auto storage = std::vector<element>(1 << 16);

  auto pointers = std::vector<element const*>();

  for (auto const& each : storage)
  {
    pointers.push_back(&each);
  }

  auto engine = std::mt19937(0);

  std::shuffle(pointers.begin(), pointers.end(), engine);

  std::cout << "std::set\t<std::array<std::uint64_t, " << N << "> const*>";

  measure<std::set<element const*>>(pointers);

  std::cout << "pointer_set\t<std::array<std::uint64_t, " << N << "> const*>";

  measure<meevax::pointer_set<element const*, meevax::cleanup::automatic, 15, 16, 16>>(pointers);

  std::cout << "multichannel_pointer_set\t<std::array<std::uint64_t, " << N << "> const*>";

  measure<meevax::multichannel_pointer_set<element>>(pointers);
}

auto main() -> int
{
  std::cout << std::fixed << std::setprecision(6);

  std::cout << "container\tparameter\tinsert-ms\tcontains-ms\tlower_bound-ms\tforward-ms\treverse-ms\terase-ms\tchecksum\n";

  compare< 1>();
  compare< 8>();
  compare<64>();
}
