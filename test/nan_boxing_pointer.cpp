#undef NDEBUG

#include <bitset>
#include <cassert>

#include <meevax/kernel/environment.hpp>
#include <meevax/utility/debug.hpp>
#include <meevax/utility/demangle.hpp>

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

  {
    let x = make<double>(3.14);

    assert(x.is<double>());
    assert(x.as<double>() == 3.14);

    x = make<double>(1.23);

    assert(x.is<double>());
    assert(x.as<double>() == 1.23);

    x = make<std::int32_t>(-42);

    assert(x.is<std::int32_t>());
    assert(x.as<std::int32_t>() == -42);

    x = make<bool>(true);

    assert(x.is<bool>());
    assert(x.as<bool>() == true);
  }

  {
    let x = make<std::int32_t>(1),
        y = make<std::int32_t>(1),
        z = make<std::int32_t>(2);

    assert(x == y);
    assert(x.compare(y));

    assert(x != z);
    assert(not x.compare(z));
  }

  {
    assert(lexical_cast<std::string>(make<double>(3.14)) == "3.14");
    assert(lexical_cast<std::string>(make<std::int32_t>(42)) == "42");
  }

  delete p;

  return EXIT_SUCCESS;
}
