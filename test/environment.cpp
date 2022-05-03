#undef NDEBUG

#include <cassert>
#include <meevax/kernel/library.hpp>

auto main() -> int
{
  using namespace meevax;

  const auto specials_count = 11;
  assert(standard_error.is<standard_error_port>());
  assert(standard_input.is<standard_input_port>());
  assert(standard_output.is<standard_output_port>());
  assert(e0.is<exact_integer>());
  assert(e1.is<exact_integer>());
  assert(eof_object.is<eof>());
  assert(eos_object.is<eos>());
  assert(f.is<boolean>());
  assert(t.is<boolean>());
  assert(undefined.is<unbound>());
  assert(unspecified_object.is<unspecified>());

  const auto gc_count = gc.count();
  assert(gc_count == constants.size() + specials_count);

  {
    bootstrap();

    auto root = environment(master);
  }

  environment::symbols.clear();
  libraries.clear();

  gc.collect();
  gc.collect(); // for vector type

  assert(environment::symbols.empty());
  assert(gc_count == gc.count());

  return EXIT_SUCCESS;
}
