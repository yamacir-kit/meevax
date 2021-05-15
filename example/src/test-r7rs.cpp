#define BOOST_TEST_MODULE test_r7rs

#include <boost/test/included/unit_test.hpp>

#include <meevax/kernel/exact_integer.hpp>
#include <meevax/kernel/floating_point.hpp>
#include <meevax/kernel/syntactic_continuation.hpp>

using namespace meevax;

struct basis
{
  syntactic_continuation root;

  explicit basis()
    : root { layer<2>() }
  {}
};

BOOST_FIXTURE_TEST_SUITE(expressions, basis); namespace
{
  BOOST_AUTO_TEST_CASE(procedures) // 4.1.4.
  {
    let f = make<closure>();

    BOOST_CHECK(f.is<closure>());
  }
}
BOOST_AUTO_TEST_SUITE_END();

BOOST_FIXTURE_TEST_SUITE(standard_procedures, basis); namespace
{
  BOOST_AUTO_TEST_CASE(numbers) // 6.2.
  {
    let x1 = make<exact_integer>(1);

    BOOST_CHECK(x1);
    BOOST_CHECK(x1.is<exact_integer>());
    BOOST_CHECK(x1.as<exact_integer>() == 1);

    let x2 = root.read("2");

    BOOST_CHECK(x2);
    BOOST_CHECK(x2.is<exact_integer>());
    BOOST_CHECK(x2.as<exact_integer>() == 2);

    let x3 = x1 + x2;

    BOOST_CHECK(x3);
    BOOST_CHECK(x3.is<exact_integer>());
    BOOST_CHECK(x3.as<exact_integer>() == 3);
  }

  BOOST_AUTO_TEST_CASE(booleans) // 6.3.
  {
    let x = make<boolean>(true);

    BOOST_CHECK(x.is<boolean>());
    BOOST_CHECK(x.as<boolean>() == true);
  }

  BOOST_AUTO_TEST_CASE(pairs_and_lists) // 6.4.
  {
    let x;

    BOOST_CHECK(x.is<null>());

    let p = cons(make<symbol>("hoge"), make<symbol>("fuga"));

    BOOST_CHECK(p.is<pair>());
  }

  BOOST_AUTO_TEST_CASE(symbols) // 6.5.
  {
    let x = make<symbol>("hoge");

    BOOST_CHECK(x);
    BOOST_CHECK(x.is<symbol>());
  }

  BOOST_AUTO_TEST_CASE(characters) // 6.6.
  {
    let c = make<character>('c');

    BOOST_CHECK(c);
    BOOST_CHECK(c.is<character>());
  }
}
BOOST_AUTO_TEST_SUITE_END();
