#include <bitset>
#include <map>

#undef NDEBUG

#include <meevax/utility/debug.hpp>
#include <meevax/memory/pointer_set.hpp>

auto main() -> int
{
  {
    meevax::memory::pointer_set<int *> set;
  }

  {
    meevax::memory::pointer_set<int *, std::map, std::bitset> set;
  }

  return EXIT_SUCCESS;
}
