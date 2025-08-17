#undef NDEBUG

#include <cassert>
#include <meevax/kernel/environment.hpp>

auto main() -> int
{
  using namespace meevax;

  let a = make<symbol>("a"),
      b = make<symbol>("b"),
      c = make<symbol>("c"),
      d = make<symbol>("d");

  assert(lexical_cast(cons(a, nullptr)) == "(a)");

  assert(lexical_cast(cons(list(a), list(b, c, d))) == "((a) b c d)");

  assert(lexical_cast(cons(make<string>("a"), list(b, c))) == "(\"a\" b c)");

  assert(lexical_cast(cons(a, make<small_integer>(3))) == "(a . 3)");

  assert(lexical_cast(cons(list(a, b), c)) == "((a b) . c)");

  assert(lexical_cast(list(a, make<small_integer>(3 + 4), c)) == "(a 7 c)");

  assert(lexical_cast(list()) == "nullptr");

  assert(lexical_cast(object(list())) == "()");

  assert(lexical_cast(xcons(list(b, c), a)) == "(a b c)");

  assert(lexical_cast(make_list(4, c)) == "(c c c c)");

  {
    let x = list(a, b, c);

    for (auto iter = x.begin(); iter != x.end(); ++iter)
    {
      assert((*iter).template is<symbol>());
    }
  }

  {
    let x = list(a, b, c);

    for (auto iter = x.begin(); iter != x.end(); ++iter)
    {
      assert(iter->template is<symbol>());
    }
  }

  {
    let x = circular_list(a, b);

    let y = circular_list(a, b, a, b);

    assert(equal(x, y));
  }

  return EXIT_SUCCESS;
}
