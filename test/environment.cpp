#undef NDEBUG

#include <cassert>
#include <meevax/basis.hpp>
#include <meevax/kernel/boot.hpp>
#include <meevax/kernel/eof.hpp>
#include <meevax/kernel/interaction_environment.hpp>
#include <meevax/kernel/library.hpp>

auto main() -> int
{
  using namespace meevax;

  const auto specials_count = 4;
  {
    assert(e0.is<std::int32_t>());
    assert(e1.is<std::int32_t>());
    assert(eof_object.is<eof>());
    assert(undefined.is<ghost>());
    assert(unspecified.is<ghost>());
    assert(interaction_environment().is<environment>());
  }

  const auto gc_count = default_collector::count();

  assert(gc_count == specials_count);

  boot();
  boot(basis());

  symbols().clear();

  assert(symbols().empty());

  const_cast<object &>(interaction_environment()).reset(); // DIRTY HACK!

  const_cast<object &>(environment::core()).reset(); // DIRTY HACK!

  libraries().clear();

  assert(libraries().empty());

  default_collector::collect();

  assert(gc_count - 1 == default_collector::count()); // -1 is interaction_environment

  return EXIT_SUCCESS;
}
