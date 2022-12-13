#undef NDEBUG

#include <cassert>
#include <meevax/kernel/environment.hpp>

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

  assert(gc.count() == gc_count);
  assert(gc.collect() == 0);
  assert(gc.count() == gc_count);

  // copy
  {
    let x = make<symbol>("hoge");
    let y = x;

    assert(x.is<symbol>());
    assert(y.is<symbol>());

    assert(eq(x, y));
    assert(eqv(x, y));
  }

  assert(gc.count() == gc_count + 1);
  assert(gc.collect() == 1);
  assert(gc.count() == gc_count);

  // move
  {
    auto f = []()
    {
      let x = make<symbol>("x");

      assert(x.is<symbol>());
      assert(x.as<symbol>() == "x");

      return x; // return value optimization
    };

    let x = f();

    assert(x.is<symbol>());
    assert(x.as<symbol>() == "x");

    assert(gc.collect() == 0);
    assert(gc.count() == gc_count + 1);

    assert(x.is<symbol>());
    assert(x.as<symbol>() == "x");
  }

  assert(gc.count() == gc_count + 1);
  assert(gc.collect() == 1);
  assert(gc.count() == gc_count);

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
      assert(gc.collect() == 0);
      assert(gc.count() == gc_count + 3);

      return list(x, y, z);
    };

    let a = f();

    assert(length(a) == 3);

    assert(a[0].is<symbol>());
    assert(a[1].is<symbol>());
    assert(a[2].is<symbol>());

    assert(gc.count() == gc_count + 6);
    assert(gc.collect() == 0);
    assert(gc.count() == gc_count + 6);

    assert(length(a) == 3);

    assert(a[0].is<symbol>());
    assert(a[1].is<symbol>());
    assert(a[2].is<symbol>());
  }

  assert(gc.count() == gc_count + 6);
  assert(gc.collect() == 6);
  assert(gc.count() == gc_count);

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

    assert(x[0].as<symbol>() == "a");
    assert(x[1].as<symbol>() == "b");
    assert(x[2].as<symbol>() == "c");
    assert(x[3].as<symbol>() == "a");

    gc.collect();

    assert(x[0].as<symbol>() == "a");
    assert(x[1].as<symbol>() == "b");
    assert(x[2].as<symbol>() == "c");
    assert(x[3].as<symbol>() == "a");
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

  // number
  {
    {
      let const x = make<double>(42.0);

      assert(x.is<double>());
      assert(x.as<double>() == 42);

      let const y = make(lexical_cast<double>("42"));

      assert(y.is<double>());
      assert(y.as<double>() == 42);
    }

    assert(gc.count() == gc_count);
    assert(gc.collect() == 0);
    assert(gc.count() == gc_count);
  }

  gc.collect();

  // read list
  {
    auto module = environment();

    const auto gc_count = gc.count();

    module.read("(a a a)");

    gc.collect();

    assert(gc.count() == gc_count + 1);
  }

  return EXIT_SUCCESS;
}
