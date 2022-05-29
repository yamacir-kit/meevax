#undef NDEBUG

#include <cassert>
#include <meevax/kernel/environment.hpp>

struct structure
{
  std::string text;

  int value;

  explicit structure(std::string const& text, int value)
    : text { text }
    , value { value }
  {}
};

auto main() -> int
{
  using namespace meevax;

  auto * p = new structure("hello, world!", 42);

  {
    tagged_pointer<structure> tp { nullptr };

    assert(tp.type() == typeid(std::nullptr_t));

    assert(tp.is<std::nullptr_t>());
  }

  {
    tagged_pointer<structure, std::uint32_t> tp { p };

    assert(tp.type() == typeid(structure *));

    assert(tp.is<structure *>());

    assert((*tp).text == "hello, world!");

    assert((*tp).value == 42);
  }

  {
    tagged_pointer<structure, std::uint32_t> tp { 42 };

    assert(tp.type() == typeid(std::uint32_t));

    assert(tp.is<std::uint32_t>());
  }

  delete p;

  return EXIT_SUCCESS;
}
