#include <meevax/kernel/syntactic_continuation.hpp>

#define DEBUG() \
  std::cerr << __FILE__ << ":" << __LINE__ << std::endl

#define TEST(TITLE, ...)                                                       \
  do                                                                           \
  {                                                                            \
    using namespace meevax::kernel;                                            \
                                                                               \
    auto result { false };                                                     \
                                                                               \
    std::cout << "case " << ++cases << ": " TITLE " => " << std::flush;        \
                                                                               \
    __VA_ARGS__                                                                \
                                                                               \
    std::cout << std::boolalpha << result << "\n";                             \
                                                                               \
    if (result)                                                                \
    {                                                                          \
      ++passed;                                                                \
    }                                                                          \
  }                                                                            \
  while (false)

int main()
{
  std::size_t cases { 0 }, passed { 0 };

  TEST("make boolean",
  {
    const auto x { make<boolean>(true) };
    result = x.is<boolean>();
  });

  TEST("make character",
  {
    const auto x { make<character>("x") };
    result = x.is<character>();
  });

  TEST("make closure",
  {
    const auto x { make<closure>() };
    result = x.is<closure>();
  });

  TEST("make exception",
  {
    const auto x { make<exception>("This is test exception") };
    result = x.is<exception>();
  });

  TEST("make symbol",
  {
    const auto x { make<symbol>("test") };
    result = x.is<symbol>();
  });

  TEST("make pair",
  {
    const object x { cons(make<symbol>("hoge"), make<symbol>("fuga")) };
    result = x.is<pair>();
  });

  TEST("make list",
  {
    const object x
    {
      list(
        make<symbol>("hoge"),
        make<symbol>("fuga"))
    };

    result = x.is<pair>();
  });

  TEST("boot layer-0",
  {
    syntactic_continuation x { layer<0>() };
    result = true;
  });

  TEST("boot layer-1",
  {
    syntactic_continuation x { layer<1>() };
    result = true;
  });

  TEST("add integer and integer",
  {
    const auto a { make<exact_integer>(1) };

    if (not a.is<exact_integer>())
    {
      throw std::logic_error { std::to_string(__LINE__) };
    }

    if (a.as<exact_integer>().value.convert_to<int>() != 1)
    {
      throw std::logic_error { std::to_string(__LINE__) };
    }

    const auto b { make<exact_integer>(2) };

    if (not b.is<exact_integer>())
    {
      throw std::logic_error { std::to_string(__LINE__) };
    }

    if (not b.as<exact_integer>().is(2))
    {
      throw std::logic_error { std::to_string(__LINE__) };
    }

    let const x = a + b;

    result = x.is<exact_integer>() && (x.as<exact_integer>().is(3));
  });

  // TEST("add number and native int",
  // {
  //   auto x { make<real>(1) + 2 };
  //   result = (x.as<real>() == 3);
  // });
  //
  // TEST("add native int and number",
  // {
  //   auto x { 1 + make<real>(2) };
  //   result = (x.as<real>() == 3);
  // });

  // TEST("boot layer-2",
  // {
  //   syntactic_continuation x { layer<2> };
  //   result = true;
  // });
  //
  // TEST("boot layer-42",
  // {
  //   syntactic_continuation x { layer<42> };
  //   result = true;
  // });

  {
    using namespace meevax::kernel;

    // static_assert(tag<void*>::value    == 0b0000);

    static_assert(category<bool>::value == 0b1101);

    static_assert(tag<float>::value    == 0b0101'1010);

    static_assert(tag<int8_t>::value   == 0b0011'1000);
    static_assert(tag<int16_t>::value  == 0b0100'1000);
    static_assert(tag<int32_t>::value  == 0b0101'1000);
    // static_assert(tag<int64_t>::value  == 0b1000);

    static_assert(tag<uint8_t>::value  == 0b0011'1100);
    static_assert(tag<uint16_t>::value == 0b0100'1100);
    static_assert(tag<uint32_t>::value == 0b0101'1100);
    // static_assert(tag<uint64_t>::value == 0b1100);
  }

  std::cout << "\n"
            << passed << " of " << cases << " cases passed the unit-test.\n";

  return cases != passed ? boost::exit_failure : boost::exit_success;
}


