#undef NDEBUG

#include <cassert>
#include <meevax/library/standard.hpp>

auto main() -> int
{
  using namespace meevax;

  assert(make<pair>().is<pair>());

  return EXIT_SUCCESS;
}
