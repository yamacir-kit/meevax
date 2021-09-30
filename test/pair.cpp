#undef NDEBUG

#include <cassert>
#include <meevax/kernel/list.hpp>

auto main() -> int
{
  using namespace meevax;

  assert(make<pair>().is<pair>());

  return EXIT_SUCCESS;
}
