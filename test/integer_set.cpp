#undef NDEBUG

#include <cassert>
#include <meevax/memory/integer_set.hpp>

auto main() -> int
{
  using namespace meevax;

  using uint64_set = meevax::integer_set<std::uint64_t, 16, 16, 16, 16>;

  auto integers = uint64_set();

  assert(integers.size() == 0);

  assert(integers.begin() == integers.end());

  assert(uint64_set::const_iterator() == integers.end());

  assert(not integers.end());

  assert(--integers.begin() == integers.end()); // SPECIAL BEHAVIOR

  integers.insert(                   0u);
  integers.insert(                  10u);
  integers.insert(                 100u);
  integers.insert(                1000u);
  integers.insert(               10000u);
  integers.insert(              100000u);
  integers.insert(             1000000u);
  integers.insert(            10000000u);
  integers.insert(           100000000u);
  integers.insert(          1000000000u);
  integers.insert(         10000000000u);
  integers.insert(        100000000000u);
  integers.insert(       1000000000000u);
  integers.insert(      10000000000000u);
  integers.insert(     100000000000000u);
  integers.insert(    1000000000000000u);
  integers.insert(   10000000000000000u);
  integers.insert(  100000000000000000u);
  integers.insert( 1000000000000000000u);
  integers.insert(10000000000000000000u);
  integers.insert(18446744073709551615u);

  assert(integers.size() == 21);

  assert(*std::next(integers.begin(),  0) ==                   0u);
  assert(*std::next(integers.begin(),  1) ==                  10u);
  assert(*std::next(integers.begin(),  2) ==                 100u);
  assert(*std::next(integers.begin(),  3) ==                1000u);
  assert(*std::next(integers.begin(),  4) ==               10000u);
  assert(*std::next(integers.begin(),  5) ==              100000u);
  assert(*std::next(integers.begin(),  6) ==             1000000u);
  assert(*std::next(integers.begin(),  7) ==            10000000u);
  assert(*std::next(integers.begin(),  8) ==           100000000u);
  assert(*std::next(integers.begin(),  9) ==          1000000000u);
  assert(*std::next(integers.begin(), 10) ==         10000000000u);
  assert(*std::next(integers.begin(), 11) ==        100000000000u);
  assert(*std::next(integers.begin(), 12) ==       1000000000000u);
  assert(*std::next(integers.begin(), 13) ==      10000000000000u);
  assert(*std::next(integers.begin(), 14) ==     100000000000000u);
  assert(*std::next(integers.begin(), 15) ==    1000000000000000u);
  assert(*std::next(integers.begin(), 16) ==   10000000000000000u);
  assert(*std::next(integers.begin(), 17) ==  100000000000000000u);
  assert(*std::next(integers.begin(), 18) == 1000000000000000000u);
  assert(*std::next(integers.begin(), 19) ==10000000000000000000u);
  assert(*std::next(integers.begin(), 20) ==18446744073709551615u);

  return EXIT_SUCCESS;
}

