#define BOOST_TEST_MODULE test_gc

#include <boost/test/included/unit_test.hpp>

#include <meevax/kernel/list.hpp>
#include <meevax/kernel/pair.hpp>
#include <meevax/kernel/symbol.hpp>

using namespace meevax;

#define DEBUG_PRINT(...) \
  std::cout << __LINE__ << ": " #__VA_ARGS__ " = " << (__VA_ARGS__) << std::endl

#define DEBUG_COLLECT() \
  std::cout << __LINE__ << ": " << gc.size() << " => (gc-collect) => " << (gc.collect(), gc.size()) << std::endl

BOOST_AUTO_TEST_CASE(no_collect)
{
  auto const size = gc.size();

  DEBUG_COLLECT();

  BOOST_CHECK(gc.size() == size);
}

BOOST_AUTO_TEST_CASE(scope)
{
  auto const size = gc.size();

  // DEBUG_PRINT(gc.size());

  {
    let x = make<symbol>("x");
    let y = make<symbol>("y");
    let z = make<symbol>("z");

    BOOST_CHECK(gc.size() == size + 3);
  }

  DEBUG_COLLECT();

  BOOST_CHECK(gc.size() == size);
}

BOOST_AUTO_TEST_CASE(copy)
{
  let x = make<symbol>("copy-x");

  // DEBUG_PRINT(x);
  BOOST_CHECK(x.is<symbol>());

  let y = x;

  // DEBUG_PRINT(y);
  BOOST_CHECK(y.is<symbol>());
  BOOST_CHECK(x.get() == y.get());

  DEBUG_COLLECT();

  // DEBUG_PRINT(x);
  // DEBUG_PRINT(y);
}

BOOST_AUTO_TEST_CASE(rvo)
{
  auto f = []()
  {
    let x = make<symbol>("rvo-x");
    BOOST_CHECK(x.is<symbol>());
    BOOST_CHECK(x.as<symbol>() == "rvo-x");
    return x;
  };

  let x = f();

  BOOST_CHECK(x.is<symbol>());
  BOOST_CHECK(x.as<symbol>() == "rvo-x");

  gc.collect();

  BOOST_CHECK(x.is<symbol>());
  BOOST_CHECK(x.as<symbol>() == "rvo-x");
}

BOOST_AUTO_TEST_CASE(proper_list)
{
  auto size = gc.size();

  auto f = []()
  {
    DEBUG_PRINT(gc.size());

    let x = make<symbol>("x");
    let y = make<symbol>("y");
    let z = make<symbol>("z");

    BOOST_CHECK(x.is<symbol>());
    BOOST_CHECK(x.as<symbol>() == "x");

    BOOST_CHECK(y.is<symbol>());
    BOOST_CHECK(y.as<symbol>() == "y");

    BOOST_CHECK(z.is<symbol>());
    BOOST_CHECK(z.as<symbol>() == "z");

    DEBUG_PRINT(gc.size());

    return list(x, y, z);
  };

  let a = f();

  DEBUG_PRINT(a);
  DEBUG_PRINT(car(a));
  DEBUG_PRINT(cadr(a));
  DEBUG_PRINT(caddr(a));

  BOOST_CHECK(length(a) == 3);

  DEBUG_COLLECT();

  BOOST_CHECK(not a.is<null>());
  BOOST_CHECK(a.is<pair>());
  BOOST_CHECK(length(a) == 3);

  DEBUG_PRINT(car(a));
  DEBUG_PRINT(cdr(a));
}
