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
    assert(nbp->value == 42);
  }

  {
    nan_boxing_pointer<structure> nbp { nullptr };

    nbp = p;

    assert(nbp.type() == typeid(structure *));
    assert(nbp.is<structure *>());
    assert((*nbp).text == "hello, world!");
    assert(nbp->value == 42);

    nbp = 3.14;

    assert(nbp.type() == typeid(double));
    assert(nbp.is<double>());
    assert(nbp.as<double>() == 3.14);
  }

  {
    nan_boxing_pointer<structure *> nbp { 3.14 };

    assert(nbp.type() == typeid(double));
    assert(nbp.is<double>());
    assert(nbp.as<double>() == 3.14);
  }

  {
    nan_boxing_pointer<structure *, std::uint32_t, std::int16_t> nbp { 3.14 };

    assert(nbp.type() == typeid(double));
    assert(nbp.is<double>());
    assert(nbp.as<double>() == 3.14);

    nbp = static_cast<std::uint32_t>(42);

    assert(nbp.type() == typeid(std::uint32_t));
    assert(nbp.is<std::uint32_t>());
    assert(nbp.as<std::uint32_t>() == 42);

    nbp = static_cast<std::int16_t>(-100);

    assert(nbp.type() == typeid(std::int16_t));
    assert(nbp.is<std::int16_t>());
    assert(nbp.as<std::int16_t>() == -100);
  }

  delete p;

  return EXIT_SUCCESS;
}
