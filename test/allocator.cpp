#include <chrono>
#include <list>
#include <map>
#include <random>
#include <vector>

#include <meevax/kernel/pair.hpp>
#include <meevax/memory/allocator.hpp>
#include <meevax/utility/debug.hpp>
#include <meevax/utility/demangle.hpp>

namespace meevax
{
  template <typename Thunk>
  auto duration(Thunk thunk)
  {
    auto begin = std::chrono::high_resolution_clock::now();
    thunk();
    return std::chrono::high_resolution_clock::now() - begin;
  }

  // TODO Remove this function once all major compilers implement std::format.
  auto operator <<(std::ostream & os, std::chrono::nanoseconds nanoseconds) -> std::ostream &
  {
    auto const fill = os.fill();

    os.fill('0');

    auto const years = std::chrono::duration_cast<std::chrono::years>(nanoseconds);

    auto const months = std::chrono::duration_cast<std::chrono::months>(nanoseconds -= years);

    auto const days = std::chrono::duration_cast<std::chrono::days>(nanoseconds -= months);

    auto const hours = std::chrono::duration_cast<std::chrono::hours>(nanoseconds -= days);

    auto const minutes = std::chrono::duration_cast<std::chrono::minutes>(nanoseconds -= hours);

    auto const seconds = std::chrono::duration_cast<std::chrono::seconds>(nanoseconds -= minutes);

    nanoseconds -= seconds;

    os << 'P'
       << std::setw(4) <<       years.count() << '-'
       << std::setw(2) <<      months.count() << '-'
       << std::setw(2) <<        days.count() << 'T'
       << std::setw(2) <<       hours.count() << ':'
       << std::setw(2) <<     minutes.count() << ':'
       << std::setw(2) <<     seconds.count() << '.'
       << std::setw(9) << nanoseconds.count();

    os.fill(fill);

    return os;
  }

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
