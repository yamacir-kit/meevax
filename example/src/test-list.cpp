#define BOOST_TEST_MODULE test_list

#include <boost/test/included/unit_test.hpp>
#include <meevax/kernel/syntactic_continuation.hpp>

using namespace meevax;

struct fixture
{
  let a, b, c, d;

  explicit fixture()
    : a { make<symbol>("a") }
    , b { make<symbol>("b") }
    , c { make<symbol>("c") }
    , d { make<symbol>("d") }
  {}

  ~fixture() = default;
};

BOOST_FIXTURE_TEST_SUITE(constructors, fixture); namespace
{
  BOOST_AUTO_TEST_CASE(cons_)
  {
    // (cons 'a '()) => (a)
    let x1 = cons(a, unit);
    BOOST_CHECK(boost::lexical_cast<std::string>(x1) == "(a)");

    // (cons '(a) '(b c d)) => ((a) b c d)
    let x2 = cons(list(a), list(b, c, d));
    BOOST_CHECK(boost::lexical_cast<std::string>(x2) == "((a) b c d)");

    // (cons "a" '(b c)) => ("a" b c)
    let x3 = cons(make<string>("a"), list(b, c));
    BOOST_CHECK(boost::lexical_cast<std::string>(x3) == "(\"a\" b c)");

    // (cons 'a 3) => (a . 3)
    let x4 = cons(a, make<exact_integer>(3));
    BOOST_CHECK(boost::lexical_cast<std::string>(x4) == "(a . 3)");

    // (cons '(a b) 'c) => ((a b) . c)
    let x5 = cons(list(a, b), c);
    BOOST_CHECK(boost::lexical_cast<std::string>(x5) == "((a b) . c)");
  }

  BOOST_AUTO_TEST_CASE(list_)
  {
    // (list 'a (+ 3 4) 'c) => (a 7 c)
    let x1 = list(a, make<exact_integer>(3 + 4), c);
    BOOST_CHECK(boost::lexical_cast<std::string>(x1) == "(a 7 c)");

    // (list) => ()
    let x2 = list();
    BOOST_CHECK(boost::lexical_cast<std::string>(x2) == "()");
  }

  BOOST_AUTO_TEST_CASE(xcons_)
  {
    // (xcons '(b c) 'a) => (a b c)
    let x1 = xcons(list(b, c), a);
    BOOST_CHECK(boost::lexical_cast<std::string>(x1) == "(a b c)");
  }

  BOOST_AUTO_TEST_CASE(make_list_)
  {
    // (make-list 4 'c) => (c c c c)
    let x1 = make_list(4, c);
    BOOST_CHECK(boost::lexical_cast<std::string>(x1) == "(c c c c)");
  }

  BOOST_AUTO_TEST_CASE(list_tabulate_)
  {
    auto make_exact_integer = [](auto&&... xs)
    {
      return make<exact_integer>(std::forward<decltype(xs)>(xs)...);
    };

    // (list-tabulate 4 values) => (0 1 2 3)
    let x1 = list_tabulate(4, make_exact_integer);
    BOOST_CHECK(boost::lexical_cast<std::string>(x1) == "(0 1 2 3)");
  }

  BOOST_AUTO_TEST_CASE(list_copy_)
  {
    let x1 = list(a, b, c);
    let x2 = list_copy(x1);
    BOOST_CHECK(boost::lexical_cast<std::string>(x2) == "(a b c)");
    BOOST_CHECK(not eq(x1, x2));
  }

  BOOST_AUTO_TEST_CASE(circular_list_)
  {
    let x = circular_list(a, b, c);

    BOOST_CHECK(car(x) == a);
    BOOST_CHECK(cadr(x) == b);
    BOOST_CHECK(caddr(x) == c);
    BOOST_CHECK(cadddr(x) == a);
  }
}
BOOST_AUTO_TEST_SUITE_END();
