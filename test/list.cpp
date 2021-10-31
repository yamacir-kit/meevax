#undef NDEBUG

#include <cassert>

#include <meevax/library/standard.hpp>

auto main() -> int
{
  using namespace meevax;

  let a = make<symbol>("a"),
      b = make<symbol>("b"),
      c = make<symbol>("c"),
      d = make<symbol>("d");

  assert(lexical_cast<std::string>(cons(a, unit)) == "(a)");
  assert(lexical_cast<std::string>(cons(list(a), list(b, c, d))) == "((a) b c d)");
  assert(lexical_cast<std::string>(cons(make<string>("a"), list(b, c))) == "(\"a\" b c)");
  assert(lexical_cast<std::string>(cons(a, make<exact_integer>(3))) == "(a . 3)");
  assert(lexical_cast<std::string>(cons(list(a, b), c)) == "((a b) . c)");

  assert(lexical_cast<std::string>(list(a, make<exact_integer>(3 + 4), c)) == "(a 7 c)");
  assert(lexical_cast<std::string>(list()) == "()");

  assert(lexical_cast<std::string>(xcons(list(b, c), a)) == "(a b c)");

  assert(lexical_cast<std::string>(make_list(4, c)) == "(c c c c)");

  assert(lexical_cast<std::string>(list_tabulate(4, [](auto&&... xs) { return make<exact_integer>(xs...); })) == "(0 1 2 3)");

  let x1 = list(a, b, c);
  let x2 = list_copy(x1);
  assert(lexical_cast<std::string>(x2) == "(a b c)");
  assert(not eq(x1, x2));

  let x = circular_list(a, b, c);
  assert(car(x) == a);
  assert(cadr(x) == b);
  assert(caddr(x) == c);
  assert(cadddr(x) == a);

  return EXIT_SUCCESS;
}
