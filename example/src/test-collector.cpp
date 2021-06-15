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
    BOOST_CHECK(size == constants.size() + 11);

    BOOST_CHECK(default_error_port.is<standard_error_port>());
    BOOST_CHECK(default_input_port.is<standard_input_port>());
    BOOST_CHECK(default_output_port.is<standard_output_port>());
    BOOST_CHECK(e0.is<exact_integer>());
    BOOST_CHECK(e1.is<exact_integer>());
    BOOST_CHECK(eof_object.is<eof>());
    BOOST_CHECK(eos_object.is<eos>());
    BOOST_CHECK(f.is<boolean>());
    BOOST_CHECK(t.is<boolean>());
    BOOST_CHECK(undefined.is<undefined_t>());
    BOOST_CHECK(unspecified.is<unspecified_t>());
  }

  ~fixture()
  {
    syntactic_continuation::symbols.clear();

    BOOST_CHECK(std::empty(syntactic_continuation::symbols));

    gc.collect();
    gc.collect();

    BOOST_CHECK(gc.size() == size);
  }
};

BOOST_FIXTURE_TEST_SUITE(features, fixture); namespace
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

  BOOST_AUTO_TEST_CASE(move)
  {
    auto f = []()
    {
      let x = make<symbol>("x");

      BOOST_CHECK(x.is<symbol>());
      BOOST_CHECK(x.as<symbol>() == "x");

      return x; //  RVO
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

      BOOST_CHECK(x.is<symbol>());
      BOOST_CHECK(y.is<symbol>());
      BOOST_CHECK(z.is<symbol>());
      BOOST_CHECK(x.as<symbol>() == "x");
      BOOST_CHECK(y.as<symbol>() == "y");
      BOOST_CHECK(z.as<symbol>() == "z");
      BOOST_CHECK(gc.size() == size + 3);

      return list(x, y, z);
    };

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

  BOOST_AUTO_TEST_CASE(improper_list)
  {
    auto f = [&]()
    {
      let a = make<symbol>("a");
      let b = make<symbol>("b");
      let c = make<symbol>("c");

      BOOST_CHECK(a.is<symbol>());
      BOOST_CHECK(b.is<symbol>());
      BOOST_CHECK(c.is<symbol>());
      BOOST_CHECK(a.as<symbol>() == "a");
      BOOST_CHECK(b.as<symbol>() == "b");
      BOOST_CHECK(c.as<symbol>() == "c");
      BOOST_CHECK(gc.size() == size + 3);

      return circular_list(a, b, c);
    };

    let x = f();

    BOOST_CHECK(car(x).as<symbol>() == "a");
    BOOST_CHECK(cadr(x).as<symbol>() == "b");
    BOOST_CHECK(caddr(x).as<symbol>() == "c");
    BOOST_CHECK(cadddr(x).as<symbol>() == "a");

    gc.collect();

    BOOST_CHECK(car(x).as<symbol>() == "a");
    BOOST_CHECK(cadr(x).as<symbol>() == "b");
    BOOST_CHECK(caddr(x).as<symbol>() == "c");
    BOOST_CHECK(cadddr(x).as<symbol>() == "a");
  }
}
BOOST_AUTO_TEST_SUITE_END();

BOOST_FIXTURE_TEST_SUITE(types, fixture); namespace
{
  using decimals = boost::mpl::list<single_float, double_float>;

  BOOST_AUTO_TEST_CASE_TEMPLATE(number, T, decimals)
  {
    let const x = make<T>(42);

    BOOST_CHECK(x.is<T>());
    BOOST_CHECK(x.as<T>() == 42);

    let const y = make<T>("42");

    BOOST_CHECK(y.is<T>());
    BOOST_CHECK(y.as<T>() == 42);
  }

  BOOST_AUTO_TEST_CASE(to_number_)
  {
    let const x = to_number("3.14", 10);
  }

  BOOST_AUTO_TEST_CASE(vector_make)
  {
    let const v = make<vector>(make<symbol>("a"),
                               make<symbol>("b"),
                               make<symbol>("c"));

    BOOST_CHECK(v.is<vector>());
    BOOST_CHECK(v.as<vector>().size() == 3);
    BOOST_CHECK(gc.size() == size + 4);

    gc.collect();

    BOOST_CHECK(v.is<vector>());
    BOOST_CHECK(v.as<vector>().size() == 3);
    BOOST_CHECK(gc.size() == size + 4);

    gc.collect();

    BOOST_CHECK(v.is<vector>());
    BOOST_CHECK(v.as<vector>().size() == 3);
    BOOST_CHECK(gc.size() == size + 4);

    v.as<vector>().clear();

    BOOST_CHECK(v.is<vector>());
    BOOST_CHECK(v.as<vector>().size() == 0);
    BOOST_CHECK(gc.size() == size + 4);

    gc.collect();

    BOOST_CHECK(v.is<vector>());
    BOOST_CHECK(v.as<vector>().size() == 0);
    BOOST_CHECK(gc.size() == size + 1);
  }

  BOOST_AUTO_TEST_CASE(vector_from_list_1)
  {
    let const v = make<vector>(for_each_in, list(make<symbol>("a"),
                                                 make<symbol>("b"),
                                                 make<symbol>("c")));

    BOOST_CHECK(v.is<vector>());
    BOOST_CHECK(v.as<vector>().size() == 3);
    BOOST_CHECK(gc.size() == size + 7);

    gc.collect();

    BOOST_CHECK(v.is<vector>());
    BOOST_CHECK(v.as<vector>().size() == 3);
    BOOST_CHECK(gc.size() == size + 4);

    gc.collect();

    BOOST_CHECK(v.is<vector>());
    BOOST_CHECK(v.as<vector>().size() == 3);
    BOOST_CHECK(gc.size() == size + 4);

    v.as<vector>().clear();

    BOOST_CHECK(v.is<vector>());
    BOOST_CHECK(v.as<vector>().size() == 0);
    BOOST_CHECK(gc.size() == size + 4);

    gc.collect();

    BOOST_CHECK(v.is<vector>());
    BOOST_CHECK(v.as<vector>().size() == 0);
    BOOST_CHECK(gc.size() == size + 1);
  }

  BOOST_AUTO_TEST_CASE(list_read)
  {
    syntactic_continuation module { layer<0>() };

    auto const size = gc.size();

    module.read("(a a a)");

    gc.collect();

    BOOST_CHECK(gc.size() == size + 1);
  }

  BOOST_AUTO_TEST_CASE(vector_from_list_2)
  {
    syntactic_continuation module { layer<0>() };

    auto const size = gc.size();

    let const v = make<vector>(for_each_in, module.read("(a b c)"));

    BOOST_CHECK(v.is<vector>());
    BOOST_CHECK(v.as<vector>().size() == 3);
    BOOST_CHECK(v.as<vector>()[0].as<symbol>() == "a");
    BOOST_CHECK(v.as<vector>()[1].as<symbol>() == "b");
    BOOST_CHECK(v.as<vector>()[2].as<symbol>() == "c");

    gc.collect();

    BOOST_CHECK(v.is<vector>());
    BOOST_CHECK(v.as<vector>().size() == 3);
    BOOST_CHECK(v.as<vector>()[0].as<symbol>() == "a");
    BOOST_CHECK(v.as<vector>()[1].as<symbol>() == "b");
    BOOST_CHECK(v.as<vector>()[2].as<symbol>() == "c");
    BOOST_CHECK(gc.size() == size + 4);

    gc.collect();

    BOOST_CHECK(v.is<vector>());
    BOOST_CHECK(v.as<vector>().size() == 3);
    BOOST_CHECK(v.as<vector>()[0].as<symbol>() == "a");
    BOOST_CHECK(v.as<vector>()[1].as<symbol>() == "b");
    BOOST_CHECK(v.as<vector>()[2].as<symbol>() == "c");
    BOOST_CHECK(gc.size() == size + 4);

    v.as<vector>().clear();

    BOOST_CHECK(v.is<vector>());
    BOOST_CHECK(v.as<vector>().size() == 0);
    BOOST_CHECK(gc.size() == size + 4);

    gc.collect();

    BOOST_CHECK(v.is<vector>());
    BOOST_CHECK(v.as<vector>().size() == 0);
  }

  BOOST_AUTO_TEST_CASE(vector_literal)
  {
    syntactic_continuation module { layer<0>() };

    auto const size = gc.size();

    let const v = module.read("#(a b c)");

    BOOST_CHECK(v.is<vector>());
    BOOST_CHECK(v.as<vector>().size() == 3);

    gc.collect();

    BOOST_CHECK(v.is<vector>());
    BOOST_CHECK(v.as<vector>().size() == 3);
    BOOST_CHECK(gc.size() == size + 4);

    gc.collect();

    BOOST_CHECK(v.is<vector>());
    BOOST_CHECK(v.as<vector>().size() == 3);
    BOOST_CHECK(gc.size() == size + 4);

    v.as<vector>().clear();

    BOOST_CHECK(v.is<vector>());
    BOOST_CHECK(v.as<vector>().size() == 0);
    BOOST_CHECK(gc.size() == size + 4);

    gc.collect();

    BOOST_CHECK(v.is<vector>());
    BOOST_CHECK(v.as<vector>().size() == 0);
    BOOST_CHECK(gc.size() == size + 4);
  }

  BOOST_AUTO_TEST_CASE(vector_evaluate)
  {
    syntactic_continuation module { layer<0>() };

    module.define<procedure>("vector", [](auto&&... xs)
    {
      return make<vector>(for_each_in, std::forward<decltype(xs)>(xs)...);
    });

    module.evaluate(module.read("(vector 1 2 3)"));
  }
}
BOOST_AUTO_TEST_SUITE_END();

BOOST_FIXTURE_TEST_SUITE(layers, fixture); namespace
{
  BOOST_AUTO_TEST_CASE(v0)
  {
    syntactic_continuation root { layer<0>() };
  }

  BOOST_AUTO_TEST_CASE(v1)
  {
    syntactic_continuation root { layer<1>() };
  }

  BOOST_AUTO_TEST_CASE(v2)
  {
    syntactic_continuation root { layer<2>() };
  }

  BOOST_AUTO_TEST_CASE(v3)
  {
    syntactic_continuation root { layer<3>() };
  }

  BOOST_AUTO_TEST_CASE(v4)
  {
    syntactic_continuation root { layer<4>() };
  }
}
BOOST_AUTO_TEST_SUITE_END();
