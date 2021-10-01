#include <type_traits>
#undef NDEBUG

#include <cassert>

#include <meevax/kernel/syntactic_continuation.hpp>

auto main() -> int
{
  using namespace meevax;

  const auto singletons_count = 8;
  assert(unexpected_character<')'>::get().message().is<string>());
  assert(unexpected_character<')'>::get().irritants().is<character>());
  assert(unexpected_character<']'>::get().message().is<string>());
  assert(unexpected_character<']'>::get().irritants().is<character>());
  assert(unexpected_character<'}'>::get().message().is<string>());
  assert(unexpected_character<'}'>::get().irritants().is<character>());
  assert(unexpected_character<'.'>::get().message().is<string>());
  assert(unexpected_character<'.'>::get().irritants().is<character>());

  const auto specials_count = 11;
  assert(default_error_port.is<standard_error_port>());
  assert(default_input_port.is<standard_input_port>());
  assert(default_output_port.is<standard_output_port>());
  assert(e0.is<exact_integer>());
  assert(e1.is<exact_integer>());
  assert(eof_object.is<eof>());
  assert(eos_object.is<eos>());
  assert(f.is<boolean>());
  assert(t.is<boolean>());
  assert(undefined.is<undefined_t>());
  assert(unspecified.is<unspecified_t>());

  const auto gc_count = gc.count();
  assert(gc_count == constants.size() + singletons_count + specials_count);

  {
    auto main = syntactic_continuation(boot_upto<layer::experimental_procedure>());
  }

  syntactic_continuation::symbols.clear();

  gc.collect();
  gc.collect();

  assert(syntactic_continuation::symbols.empty());
  assert(gc_count == gc.count());

  return EXIT_SUCCESS;
}
