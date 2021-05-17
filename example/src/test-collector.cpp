#define BOOST_TEST_MODULE test_collector

#include <boost/test/included/unit_test.hpp>
#include <meevax/kernel/syntactic_continuation.hpp>

using namespace meevax;

#define DEBUG_PRINT(...) \
  std::cout << __LINE__ << ":\t" #__VA_ARGS__ " = " << (__VA_ARGS__) << std::endl

#define DEBUG_COLLECT() \
  std::cout << __LINE__ << ":\t" << gc.size() << " => (gc-collect) => " << (gc.collect(), gc.size()) << std::endl

struct fixture // Check if all allocated objects are collected.
{
  std::size_t const size;

  explicit fixture()
    : size { gc.size() }
  {
    BOOST_CHECK(size == 6);

    BOOST_CHECK(eof_object.is<eof>());
    BOOST_CHECK(eos_object.is<eos>());
    BOOST_CHECK(f.is<boolean>());
    BOOST_CHECK(t.is<boolean>());
    BOOST_CHECK(undefined.is<undefined_t>());
    BOOST_CHECK(unspecified.is<unspecified_t>());
  }

  ~fixture()
  {
    gc.collect();

    BOOST_CHECK(gc.size() == size);
  }
};

BOOST_FIXTURE_TEST_SUITE(suite, fixture); namespace
{
  BOOST_AUTO_TEST_CASE(scope)
  {
    let x = make<symbol>("x");
    let y = make<symbol>("y");
    let z = make<symbol>("z");

    BOOST_CHECK(gc.size() == size + 3);
  }

  BOOST_AUTO_TEST_CASE(copy)
  {
    let x = make<symbol>("original");
    let y = x;

    BOOST_CHECK(x.is<symbol>());
    BOOST_CHECK(y.is<symbol>());
    BOOST_CHECK(eq(x, y));
    BOOST_CHECK(eqv(x, y));
  }

  BOOST_AUTO_TEST_CASE(return_value)
  {
    auto f = []()
    {
      let x = make<symbol>("x");
      BOOST_CHECK(x.is<symbol>());
      BOOST_CHECK(x.as<symbol>() == "x");
      return x;
    };

    let x = f();

    BOOST_CHECK(x.is<symbol>());
    BOOST_CHECK(x.as<symbol>() == "x");

    gc.collect();

    BOOST_CHECK(x.is<symbol>());
    BOOST_CHECK(x.as<symbol>() == "x");
  }

  BOOST_AUTO_TEST_CASE(proper_list)
  {
    auto f = [&]()
    {
      let x = make<symbol>("x");
      let y = make<symbol>("y");
      let z = make<symbol>("z");

      BOOST_CHECK(x.is<symbol>() and x.as<symbol>() == "x");
      BOOST_CHECK(y.is<symbol>() and y.as<symbol>() == "y");
      BOOST_CHECK(z.is<symbol>() and z.as<symbol>() == "z");
      BOOST_CHECK(gc.size() == size + 3);

      return list(x, y, z);
    };

    {
      let a = f();

      BOOST_CHECK(length(a) == 3);
      BOOST_CHECK(car(a).is<symbol>());
      BOOST_CHECK(cadr(a).is<symbol>());
      BOOST_CHECK(caddr(a).is<symbol>());

      gc.collect();

      BOOST_CHECK(length(a) == 3);
      BOOST_CHECK(car(a).is<symbol>());
      BOOST_CHECK(cadr(a).is<symbol>());
      BOOST_CHECK(caddr(a).is<symbol>());
    }
  }
}
BOOST_AUTO_TEST_SUITE_END();
