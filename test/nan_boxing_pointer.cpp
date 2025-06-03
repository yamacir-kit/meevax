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

    x = make<small_integer>(-42);

    assert(x.is<small_integer>());
    assert(x.as<small_integer>() == -42);

    x = make<bool>(true);

    assert(x.is<bool>());
    assert(x.as<bool>() == true);
  }

  {
    let x = make<small_integer>(1),
        y = make<small_integer>(1),
        z = make<small_integer>(2);

    assert(x.is<small_integer>());
    assert(y.is<small_integer>());
    assert(z.is<small_integer>());

    assert(x == y);
    assert(x.compare(y));

    assert(x != z);
    assert(not x.compare(z));
  }

  {
    assert(lexical_cast<std::string>(make<double>(3.14)) == "3.14");
    assert(lexical_cast<std::string>(make<small_integer>(42)) == "42");
  }

  {
    let x = make(static_cast<small_integer>(1)),
        y = make(static_cast<small_integer>(2));

    // PRINT(demangle(x.type()));
    // PRINT(demangle(y.type()));

    assert(x.is<small_integer>());
    assert(y.is<small_integer>());

    let const pare = cons(x, y);

    PRINT(demangle(pare.as<pair>().first.type()));
    PRINT(demangle(pare.as<pair>().second.type()));

    assert(pare.as<pair>().first.is<small_integer>());
    assert(pare.as<pair>().first.as<small_integer>() == 1);
    assert(pare.as<pair>().second.is<small_integer>());
    assert(pare.as<pair>().second.as<small_integer>() == 2);

    assert(car(pare).is<small_integer>());
    assert(car(pare).as<small_integer>() == 1);
    assert(cdr(pare).is<small_integer>());
    assert(cdr(pare).as<small_integer>() == 2);
  }

  delete p;

  return EXIT_SUCCESS;
}
