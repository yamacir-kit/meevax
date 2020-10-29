#include <meevax/kernel/exact_integer.hpp>
#include <meevax/kernel/floating_point.hpp>
#define BOOST_TEST_MODULE Example

#include <boost/test/included/unit_test.hpp>

#include <meevax/kernel/syntactic_continuation.hpp>

struct basis
{
  meevax::syntactic_continuation root;

  explicit basis()
    : root { meevax::layer<2>() }
  {}
};

using meevax::let;
using meevax::make;

BOOST_FIXTURE_TEST_SUITE(Expressions, basis); // 4.

BOOST_AUTO_TEST_CASE(Procedures) // 4.1.4.
{
  using meevax::closure;

  let f = make<closure>();

  BOOST_CHECK(f.is<closure>());
}

BOOST_AUTO_TEST_SUITE_END();

BOOST_FIXTURE_TEST_SUITE(Standard_procedures, basis); // 6.

BOOST_AUTO_TEST_CASE(Numbers) // 6.2.
{
  using meevax::exact_integer;
  using meevax::ratio;
  using meevax::default_float;

  let x1 = make<exact_integer>(1);

  BOOST_CHECK(x1);
  BOOST_CHECK(x1.is<exact_integer>());
  BOOST_CHECK(x1.as<exact_integer>().is(1));

  let x2 = root.read("2");

  BOOST_CHECK(x2);
  BOOST_CHECK(x2.is<exact_integer>());
  BOOST_CHECK(x2.as<exact_integer>().is(2));

  let x3 = x1 + x2;

  BOOST_CHECK(x3);
  BOOST_CHECK(x3.is<exact_integer>());
  BOOST_CHECK(x3.as<exact_integer>().is(3));
}

BOOST_AUTO_TEST_CASE(Booleans) // 6.3.
{
  using meevax::boolean;

  let x = make<boolean>(true);

  BOOST_CHECK(x.is<boolean>());
  BOOST_CHECK(x.as<boolean>() == true);
}

BOOST_AUTO_TEST_CASE(Pairs_and_lists) // 6.4.
{
  using meevax::null;

  let x;

  BOOST_CHECK(x.is<null>());

  using meevax::pair;
  using meevax::cons;
  using meevax::symbol;

  let p = cons(make<symbol>("hoge"), make<symbol>("fuga"));

  BOOST_CHECK(p.is<pair>());
}

BOOST_AUTO_TEST_CASE(Symbols) // 6.5.
{
  using meevax::symbol;

  let x = make<symbol>("hoge");

  BOOST_CHECK(x);
  BOOST_CHECK(x.is<symbol>());
}

BOOST_AUTO_TEST_CASE(Characters) // 6.6.
{
  using meevax::character;

  let c = make<character>("c");

  BOOST_CHECK(c);
  BOOST_CHECK(c.is<character>());
}

BOOST_AUTO_TEST_SUITE_END();
