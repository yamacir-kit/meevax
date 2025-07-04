#undef NDEBUG

#include <cassert>
#include <vector>

#include <meevax/memory/pointer_set.hpp>
#include <meevax/utility/debug.hpp>

auto main() -> int
{
  using namespace meevax;

  using int_pointer_set = meevax::pointer_set<int const*, 15, 16, 16>;

  auto integers = int_pointer_set();

  assert(integers.size() == 0);

  assert(integers.begin() == integers.end());

  assert(int_pointer_set::const_iterator() == integers.end());

  assert(not integers.end());

  assert(--integers.end() == integers.end());

  assert(--integers.begin() == integers.end()); // SPECIAL BEHAVIOR

  auto v = std::vector<int const*>();

  for (auto i = 0; i < 1000; ++i)
  {
    v.push_back(new int(i));
  }

  for (auto i : v)
  {
    integers.insert(i);
  }

  LINE();
  PRINT(integers.begin().p);
  PRINT(integers.begin().i);
  PRINT(integers.begin().sub.p);
  PRINT(integers.begin().sub.i);
  PRINT(integers.begin().sub.sub.p);
  PRINT(integers.begin().sub.sub.i);

  LINE();
  PRINT(integers.end().p);
  PRINT(integers.end().i);
  PRINT(integers.end().sub.p);
  PRINT(integers.end().sub.i);
  PRINT(integers.end().sub.sub.p);
  PRINT(integers.end().sub.sub.i);

  assert(integers.begin() != integers.end());

  LINE();
  auto prev_begin = --integers.begin();

  LINE();
  PRINT(prev_begin.p);
  PRINT(prev_begin.i);
  PRINT(prev_begin.sub.p);
  PRINT(prev_begin.sub.i);
  PRINT(prev_begin.sub.sub.p);
  PRINT(prev_begin.sub.sub.i);

  LINE();
  PRINT(prev_begin.p         == integers.end().p);
  PRINT(prev_begin.i         == integers.end().i);
  PRINT(prev_begin.sub.p     == integers.end().sub.p);
  PRINT(prev_begin.sub.i     == integers.end().sub.i);
  PRINT(prev_begin.sub.sub.p == integers.end().sub.sub.p);
  PRINT(prev_begin.sub.sub.i == integers.end().sub.sub.i);

  assert(prev_begin == integers.end());

  for (auto p : integers)
  {
    delete p;
  }

  return EXIT_SUCCESS;
}
