#undef NDEBUG

#include <cassert>
#include <meevax/memory/nan_boxing_pointer.hpp>
#include <meevax/utility/debug.hpp>

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
    nan_boxing_pointer<structure> nbp { nullptr };

    assert(nbp.type() == typeid(structure *));
    assert(nbp.is<structure *>());
  }

  {
    nan_boxing_pointer<structure> nbp { p };

    assert(nbp.type() == typeid(structure *));
    assert(nbp.is<structure *>());
    assert((*nbp).text == "hello, world!");
    assert((*nbp).value == 42);
  }

  {
    nan_boxing_pointer<structure> nbp { nullptr };

    nbp = p;

    assert(nbp.type() == typeid(structure *));
    assert(nbp.is<structure *>());
    assert((*nbp).text == "hello, world!");
    assert((*nbp).value == 42);
  }

  delete p;

  return EXIT_SUCCESS;
}
