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

  assert(lexical_cast<external_representation>(cons(a, unit)) == "(a)");
  assert(lexical_cast<external_representation>(cons(list(a), list(b, c, d))) == "((a) b c d)");
  assert(lexical_cast<external_representation>(cons(make<string>("a"), list(b, c))) == "(\"a\" b c)");
  assert(lexical_cast<external_representation>(cons(a, make<exact_integer>(3))) == "(a . 3)");
  assert(lexical_cast<external_representation>(cons(list(a, b), c)) == "((a b) . c)");

  assert(lexical_cast<external_representation>(list(a, make<exact_integer>(3 + 4), c)) == "(a 7 c)");
  assert(lexical_cast<external_representation>(list()) == "()");

  assert(lexical_cast<external_representation>(xcons(list(b, c), a)) == "(a b c)");

  assert(lexical_cast<external_representation>(make_list(4, c)) == "(c c c c)");

  assert(lexical_cast<external_representation>(list_tabulate(4, [](auto&&... xs) { return make<exact_integer>(xs...); })) == "(0 1 2 3)");

  let x1 = list(a, b, c);
  let x2 = list_copy(x1);
  assert(lexical_cast<external_representation>(x2) == "(a b c)");
  assert(not eq(x1, x2));

  let x = circular_list(a, b, c);
  assert(car(x) == a);
  assert(cadr(x) == b);
  assert(caddr(x) == c);
  assert(cadddr(x) == a);

  {
    let x = list(a, b, c);

    for (auto iter = std::begin(x); iter != std::end(x); ++iter)
    {
      assert((*iter).template is<symbol>());
    }
  }

  {
    let x = list(a, b, c);

    for (auto iter = std::begin(x); iter != std::end(x); ++iter)
    {
      assert(iter->template is<symbol>());
    }
  }

  return EXIT_SUCCESS;
}
