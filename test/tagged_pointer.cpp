#undef NDEBUG

#include <cassert>
#include <meevax/memory/tagged_pointer.hpp>

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

    assert(tp.type() == typeid(structure *));
    assert(tp.is<structure *>());
  }

  {
    tagged_pointer<structure, std::uint32_t> tp { p };

    assert(tp.type() == typeid(structure *));
    assert(tp.is<structure *>());
    assert((*tp).text == "hello, world!");
    assert(tp->value == 42);
  }

  {
    tagged_pointer<structure, std::uint32_t, float> tp { static_cast<std::uint32_t>(42) };

    assert(tp.type() == typeid(std::uint32_t));
    assert(tp.is<std::uint32_t>());
    assert(tp.as<std::uint32_t>() == 42);

    tp = 3.14f;

    assert(tp.type() == typeid(float));
    assert(tp.is<float>());
    assert(tp.as<float>() == 3.14f);
  }

  delete p;

  return EXIT_SUCCESS;
}
