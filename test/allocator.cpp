#include <chrono>
#include <list>
#include <map>
#include <random>
#include <vector>

#include <meevax/chrono/duration.hpp>
#include <meevax/kernel/pair.hpp>
#include <meevax/memory/allocator.hpp>
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

template <typename T, typename Allocator>
auto measure()
{
  auto result = meevax::result(meevax::demangle(typeid(Allocator).name()));

  auto v = std::list<T, Allocator>();

  static auto const xs = std::vector<T>(std::pow(2, 16));

  result.emplace("push_back", meevax::duration([&]()
  {
    for (auto x : xs)
    {
      v.push_back(x);
    }
  }));

  return result;
}

struct hoge
{
  std::int64_t a, b, c, d;
};

auto main() -> int
{
  std::cout << measure<int, std::allocator<int>>() << std::endl;
  std::cout << measure<int, meevax::allocator<int>>() << std::endl;

  std::cout << measure<meevax::pair, meevax::allocator<meevax::pair>>() << std::endl;

  std::cout << measure<std::array<std::uint8_t,  1>, meevax::allocator<std::array<std::uint8_t,  1>>>() << std::endl;
  std::cout << measure<std::array<std::uint8_t,  2>, meevax::allocator<std::array<std::uint8_t,  2>>>() << std::endl;
  std::cout << measure<std::array<std::uint8_t,  4>, meevax::allocator<std::array<std::uint8_t,  4>>>() << std::endl;
  std::cout << measure<std::array<std::uint8_t,  8>, meevax::allocator<std::array<std::uint8_t,  8>>>() << std::endl;
  std::cout << measure<std::array<std::uint8_t, 16>, meevax::allocator<std::array<std::uint8_t, 16>>>() << std::endl;

  return EXIT_SUCCESS;
}
