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

  return EXIT_SUCCESS;
}

