#undef NDEBUG

#include <cassert>
#include <meevax/kernel/syntactic_continuation.hpp>

auto main() -> int
{
  using namespace meevax;

  assert(make<pair>().is<pair>());

  return EXIT_SUCCESS;
}
