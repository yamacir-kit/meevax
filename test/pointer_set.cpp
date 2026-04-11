#undef NDEBUG

#include <cassert>
#include <vector>

#include <meevax/memory/pointer_set.hpp>
#include <meevax/utility/debug.hpp>

auto main() -> int
{
  using namespace meevax;

  using int_pointer_set = meevax::pointer_set<int const*, 15, 16, 16>;

  auto int_pointers = int_pointer_set();

  assert(int_pointers.size() == 0);

  assert(int_pointers.begin() == int_pointers.end());

  assert(int_pointer_set::const_iterator() == int_pointers.end());

  assert(not int_pointers.end());

  assert(--int_pointers.end() == int_pointers.end());

  assert(--int_pointers.begin() == int_pointers.end()); // SPECIAL BEHAVIOR

  auto constexpr size = 1000000;

  for (auto i = 0; i < size; ++i)
  {
    int_pointers.insert(reinterpret_cast<int *>(i * alignof(int)));
  }

  assert(int_pointers.size() == size);

  assert(int_pointers.begin() != int_pointers.end());

  assert(--int_pointers.begin() == int_pointers.end());

  assert(std::distance(int_pointers.begin(), int_pointers.end()) == size);

  for (auto i = 0; i < size; ++i)
  {
    int_pointers.erase(reinterpret_cast<int *>(i * alignof(int)));
  }

  assert(int_pointers.size() == 0);

  assert(int_pointers.begin() == int_pointers.end());

  assert(std::distance(int_pointers.begin(), int_pointers.end()) == 0);

  return EXIT_SUCCESS;
}
