#undef NDEBUG

#include <cassert>
#include <meevax/kernel/interaction_environment.hpp>
#include <meevax/kernel/library.hpp>

auto main() -> int
{
  using namespace meevax;

  const auto specials_count = 9;
  {
    assert(standard_error.is<standard_error_port>());
    assert(standard_input.is<standard_input_port>());
    assert(standard_output.is<standard_output_port>());
    assert(e0.is<exact_integer>());
    assert(e1.is<exact_integer>());
    assert(eof_object.is<eof>());
    assert(undefined.is<ghost>());
    assert(unspecified.is<ghost>());
    assert(interaction_environment().is<environment>());
  }

  const auto gc_count = gc.count();

  assert(gc_count == specials_count);

  library::boot();

  environment::symbols.clear();

  assert(environment::symbols.empty());

  const_cast<reference>(interaction_environment()).reset(); // DIRTY HACK!

  libraries.clear();

  gc.collect();
  gc.collect(); // for vector type

  assert(gc_count - 1 == gc.count()); // -1 is interaction_environment

  return EXIT_SUCCESS;
}
