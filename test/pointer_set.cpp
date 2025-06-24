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
  PRINT(integers.begin().iter.p);
  PRINT(integers.begin().iter.i);
  PRINT(integers.begin().iter.iter.p);
  PRINT(integers.begin().iter.iter.i);

  LINE();
  PRINT(integers.end().p);
  PRINT(integers.end().i);
  PRINT(integers.end().iter.p);
  PRINT(integers.end().iter.i);
  PRINT(integers.end().iter.iter.p);
  PRINT(integers.end().iter.iter.i);

  assert(integers.begin() != integers.end());

  LINE();
  auto prev_begin = --integers.begin();

  LINE();
  PRINT(prev_begin.p);
  PRINT(prev_begin.i);
  PRINT(prev_begin.iter.p);
  PRINT(prev_begin.iter.i);
  PRINT(prev_begin.iter.iter.p);
  PRINT(prev_begin.iter.iter.i);

  LINE();
  PRINT(prev_begin.p           == integers.end().p);
  PRINT(prev_begin.i           == integers.end().i);
  PRINT(prev_begin.iter.p      == integers.end().iter.p);
  PRINT(prev_begin.iter.i      == integers.end().iter.i);
  PRINT(prev_begin.iter.iter.p == integers.end().iter.iter.p);
  PRINT(prev_begin.iter.iter.i == integers.end().iter.iter.i);

  assert(prev_begin == integers.end());

  for (auto p : integers)
  {
    delete p;
  }

  return EXIT_SUCCESS;
}
