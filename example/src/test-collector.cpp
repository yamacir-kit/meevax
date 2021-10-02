#define BOOST_TEST_MODULE test_collector

#include <boost/mpl/list.hpp>
#include <boost/test/included/unit_test.hpp>
#include <meevax/kernel/syntactic_continuation.hpp>

using namespace meevax;

#define DEBUG_PRINT(...) \
  std::cout << __LINE__ << ":\t" #__VA_ARGS__ " = " << (__VA_ARGS__) << std::endl

#define DEBUG_COLLECT() \
  std::cout << __LINE__ << ":\t" << gc.count() << " => (gc-collect) => " << (gc.collect(), gc.count()) << std::endl

struct fixture // Check if all allocated objects are collected.
{
  std::size_t const size;

  explicit fixture()
    : size { gc.count() }
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

    BOOST_CHECK(gc.count() == size);
  }
};

BOOST_FIXTURE_TEST_SUITE(types, fixture); namespace
{
  using decimals = boost::mpl::list<f32, f64>;

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
    let const x = string_to::number("3.14", 10);
  }

  BOOST_AUTO_TEST_CASE(list_read)
  {
    syntactic_continuation module { boot_upto<layer::module_system>() };

    auto const size = gc.count();

    module.read("(a a a)");

    gc.collect();

    BOOST_CHECK(gc.count() == size + 1);
  }
}
BOOST_AUTO_TEST_SUITE_END();
