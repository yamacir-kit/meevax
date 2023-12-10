#undef NDEBUG

#include <cassert>
#include <meevax/kernel/environment.hpp>
#include <meevax/memory/simple_allocator.hpp>

auto main() -> int
{
  using namespace meevax;

  assert(make<pair>().is<pair>());

  assert((make<pair, std::allocator<void>>().is<pair>()));

  assert((make<pair, meevax::simple_allocator<void>>().is<pair>()));

  /*
     Without this explicit call to primary_collector().clear(), illegal memory
     access by the destructor of the collector will occur for objects allocated
     by allocators other than std::allocator. See the comments attached to
     collector::tagged::allocator for a detailed description of this problem.
  */
  primary_collector().clear();

  return EXIT_SUCCESS;
}
