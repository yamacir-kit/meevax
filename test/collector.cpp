#undef NDEBUG

#include <cassert>
#include <meevax/kernel/syntactic_continuation.hpp>

auto main() -> int
{
  using namespace meevax;

  const auto gc_count = gc.count();

  // scope
  {
    {
      let x = make<symbol>("x");
      let y = make<symbol>("y");
      let z = make<symbol>("z");

      assert(gc.count() == gc_count + 3);
      assert(gc.collect() == 0);
      assert(gc.count() == gc_count + 3);
    }

    assert(gc.count() == gc_count + 3);
    assert(gc.collect() == 3);
    assert(gc.count() == gc_count);
  }

  gc.collect();

  // copy
  {
    let x = make<symbol>("hoge");
    let y = x;

    assert(x.is<symbol>());
    assert(y.is<symbol>());
    assert(eq(x, y));
    assert(eqv(x, y));
  }

  gc.collect();

  // move
  {
    auto f = []()
    {
      let x = make<symbol>("x");

      assert(x.is<symbol>());
      assert(x.as<symbol>() == "x");

      return x; //  RVO
    };

    let x = f();

    assert(x.is<symbol>());
    assert(x.as<symbol>() == "x");

    gc.collect();

    assert(x.is<symbol>());
    assert(x.as<symbol>() == "x");
  }

  gc.collect();

  // proper list
  {
    auto f = [&]()
    {
      let x = make<symbol>("x");
      let y = make<symbol>("y");
      let z = make<symbol>("z");

      assert(x.is<symbol>());
      assert(y.is<symbol>());
      assert(z.is<symbol>());
      assert(x.as<symbol>() == "x");
      assert(y.as<symbol>() == "y");
      assert(z.as<symbol>() == "z");
      assert(gc.count() == gc_count + 3);

      return list(x, y, z);
    };

    let a = f();

    assert(length(a) == 3);
    assert(car(a).is<symbol>());
    assert(cadr(a).is<symbol>());
    assert(caddr(a).is<symbol>());

    gc.collect();

    assert(length(a) == 3);
    assert(car(a).is<symbol>());
    assert(cadr(a).is<symbol>());
    assert(caddr(a).is<symbol>());
  }

  gc.collect();

  // improper list
  {
    auto f = [&]()
    {
      let a = make<symbol>("a");
      let b = make<symbol>("b");
      let c = make<symbol>("c");

      assert(a.is<symbol>());
      assert(b.is<symbol>());
      assert(c.is<symbol>());
      assert(a.as<symbol>() == "a");
      assert(b.as<symbol>() == "b");
      assert(c.as<symbol>() == "c");
      assert(gc.count() == gc_count + 3);

      return circular_list(a, b, c);
    };

    let x = f();

    assert(car(x).as<symbol>() == "a");
    assert(cadr(x).as<symbol>() == "b");
    assert(caddr(x).as<symbol>() == "c");
    assert(cadddr(x).as<symbol>() == "a");

    gc.collect();

    assert(car(x).as<symbol>() == "a");
    assert(cadr(x).as<symbol>() == "b");
    assert(caddr(x).as<symbol>() == "c");
    assert(cadddr(x).as<symbol>() == "a");
  }

  gc.collect();

  // change class
  {
    let x = make<symbol>("hoge");

    gc.collect();

    assert(gc.count() == gc_count + 1);
    assert(x.is<symbol>());
    assert(x.as<symbol>() == "hoge");

    x = make<exact_integer>(42);

    gc.collect();

    assert(gc.count() == gc_count + 1);
    assert(x.is<exact_integer>());
    assert(x.as<exact_integer>() == 42);
  }

  gc.collect();

  return EXIT_SUCCESS;
}
