#undef NDEBUG

#include <algorithm>
#include <cassert>
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

  assert(cons(a, nullptr).external_representation() == "(a)");

  assert(cons(list(a), list(b, c, d)).external_representation() == "((a) b c d)");

  assert(cons(make<string>("a"), list(b, c)).external_representation() == "(\"a\" b c)");

  assert(cons(a, make<small_integer>(3)).external_representation() == "(a . 3)");

  assert(cons(list(a, b), c).external_representation() == "((a b) . c)");

  assert(list(a, make<small_integer>(3 + 4), c).external_representation() == "(a 7 c)");

  assert(unit.external_representation() == "()");

  assert(xcons(list(b, c), a).external_representation() == "(a b c)");

  assert(make_list(4, c).external_representation() == "(c c c c)");

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
