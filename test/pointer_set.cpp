#include <bitset>
#include <chrono>
#include <map>
#include <random>
#include <vector>

#include <meevax/chrono/duration.hpp>
#include <meevax/memory/integer_set.hpp>
#include <meevax/memory/pointer_set.hpp>
#include <meevax/utility/debug.hpp>
#include <meevax/utility/demangle.hpp>

namespace meevax
{
  struct result : public std::map<std::string, std::chrono::nanoseconds>
  {
    std::string name;

    explicit result(std::string const& name)
      : name { name }
    {}
  };

  auto operator <<(std::ostream & os, result const& result) -> std::ostream &
  {
    os << result.name << std::endl;

    for (auto&& [topic, period] : result)
    {
      os << "  " << topic << ":\t" << period << std::endl;
    }

    return os;
  }
}

template <typename PointerSet>
auto measure()
{
  auto result = meevax::result(meevax::demangle(typeid(PointerSet).name()));

  auto pointer_set = PointerSet();

  static auto const xs = []()
  {
    auto v = std::vector<int>(std::pow(2, 20));
    std::iota(v.begin(), v.end(), 0);
    return v;
  }();

  static auto const ps = [&]()
  {
    auto v = std::vector<int const*>(xs.size());

    for (std::size_t i = 0; i < v.size(); ++i)
    {
      v[i] = &xs[i];
    }

    auto device = std::random_device();

    auto engine = std::default_random_engine(device());

    std::shuffle(v.begin(), v.end(), engine);

    return v;
  }();

  result.emplace("insert", meevax::duration([&]()
  {
    for (auto p : ps)
    {
      pointer_set.insert(p);
    }
  }));

  assert(pointer_set.size() == ps.size());

  result.emplace("iterate", meevax::duration([&]()
  {
    [[maybe_unused]] auto i = 0;

    for ([[maybe_unused]] auto p : pointer_set)
    {
      [[maybe_unused]] volatile auto hack = 0;
      assert(*p == xs[i++]);
    }
  }));

  result.emplace("lower_bound", meevax::duration([&]()
  {
    for (auto p : ps)
    {
      [[maybe_unused]] volatile auto hack = 0;
      [[maybe_unused]] auto iter = pointer_set.lower_bound(p);
      assert(iter != pointer_set.end());
      assert(*iter == p);
    }
  }));

  result.emplace("erase", meevax::duration([&]()
  {
    for (auto p : ps)
    {
      pointer_set.erase(p);
    }
  }));

  assert(pointer_set.size() == 0);

  return result;
}

auto main() -> int
{
  std::cout << measure<meevax::v1::pointer_set<int const*, meevax::simple_flat_map, meevax::simple_bitset>>() << std::endl;
  std::cout << measure<meevax::v1::pointer_set<int const*, meevax::simple_flat_map,           std::bitset>>() << std::endl;
  std::cout << measure<meevax::v1::pointer_set<int const*,                std::map, meevax::simple_bitset>>() << std::endl;
  std::cout << measure<meevax::v1::pointer_set<int const*,                std::map,           std::bitset>>() << std::endl;

  std::cout << measure<meevax::v2::pointer_set<int const*>>() << std::endl;

  return EXIT_SUCCESS;
}
