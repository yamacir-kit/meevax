#undef NDEBUG

#include <cassert>
#include <meevax/kernel/environment.hpp>

auto main() -> int
{
  using namespace meevax;

  {
    tagged_pointer<void> tp { nullptr };

    assert(tp.type() == typeid(std::nullptr_t));

    assert(tp.is<std::nullptr_t>());
  }

  return EXIT_SUCCESS;
}
