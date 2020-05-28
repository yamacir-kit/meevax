#include <meevax/kernel/syntactic_continuation.hpp>

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

int main(int argc, char** argv)
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
    syntactic_continuation x { layer<0> };
    result = true;
  });

  TEST("boot layer-1",
  {
    syntactic_continuation x { layer<1> };
    result = true;
  });

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

  std::cout << "\n"
            << passed << " of " << cases << " cases passed the unit-test.\n";

  return cases != passed ? boost::exit_failure : boost::exit_success;
}

