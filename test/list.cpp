#undef NDEBUG

#include <cassert>
#include <meevax/iostream/lexical_cast.hpp>
#include <meevax/kernel/environment.hpp>
#include <meevax/kernel/proper_list.hpp>
#include <meevax/kernel/symbol.hpp>

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
    let xs = list(a, b, c);

    auto v = xs | as_proper_list;

    for (auto iter = v.begin(); iter != v.end(); ++iter)
    {
      assert((*iter).template is<symbol>());
    }
  }

  {
    let const xs = list(a, b, c);

    auto v = xs | as_proper_list;

    for (auto iter = v.begin(); iter != v.end(); ++iter)
    {
      assert(iter->template is<symbol>());
    }
  }

  {
    let xs = list(a, b, c);

    for (auto x : xs | as_proper_list)
    {
      assert(x.template is<symbol>());
    }
  }

  {
    let xs = list(a, b, c);

    std::ranges::for_each(xs | as_proper_list, [](let const& x)
    {
      assert(x.is<symbol>());
    });
  }

  {
    let x = circular_list(a, b);

    let y = circular_list(a, b, a, b);

    assert(equal(x, y));
  }

  return EXIT_SUCCESS;
}
