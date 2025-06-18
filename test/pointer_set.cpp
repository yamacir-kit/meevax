#undef NDEBUG

#include <cassert>
#include <meevax/memory/pointer_set.hpp>

auto main() -> int
{
  using namespace meevax;

  using int_pointer_set = meevax::pointer_set<int *, 16, 16, 16>;

  auto integers = int_pointer_set();

  assert(integers.size() == 0);

  assert(integers.begin() == integers.end());

  assert(int_pointer_set::const_iterator() == integers.end());

  assert(not integers.end());

  assert(--integers.begin() == integers.end()); // SPECIAL BEHAVIOR

  return EXIT_SUCCESS;
}

