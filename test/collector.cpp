#undef NDEBUG

#include <cassert>
#include <meevax/kernel/environment.hpp>

auto main() -> int
{
  using namespace meevax;

  const auto gc_count = default_collector::count();

  // scope
  {
    {
      let x = make<symbol>("x");
      let y = make<symbol>("y");
      let z = make<symbol>("z");

      assert(default_collector::count() == gc_count + 3);
      default_collector::collect();
      assert(default_collector::count() == gc_count + 3);
    }

    assert(default_collector::count() == gc_count + 3);
    default_collector::collect();
    assert(default_collector::count() == gc_count);
  }

  assert(default_collector::count() == gc_count);
  default_collector::collect();
  assert(default_collector::count() == gc_count);

  // copy
  {
    let x = make<symbol>("hoge");
    let y = x;

    assert(x.is<symbol>());
    assert(y.is<symbol>());

    assert(eq(x, y));
    assert(eqv(x, y));
  }

  assert(default_collector::count() == gc_count + 1);
  default_collector::collect();
  assert(default_collector::count() == gc_count);

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

    assert(default_collector::count() == gc_count + 1);
    default_collector::collect();
    assert(default_collector::count() == gc_count + 1);

    assert(x.is<symbol>());
    assert(x.as<symbol>() == "x");
  }

  assert(default_collector::count() == gc_count + 1);
  default_collector::collect();
  assert(default_collector::count() == gc_count);

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

      assert(default_collector::count() == gc_count + 3);
      default_collector::collect();
      assert(default_collector::count() == gc_count + 3);

      return list(x, y, z);
    };

    let a = f();

    assert(length(a) == 3);

    assert(car(a).is<symbol>());
    assert(cadr(a).is<symbol>());
    assert(caddr(a).is<symbol>());

    assert(default_collector::count() == gc_count + 6);
    default_collector::collect();
    assert(default_collector::count() == gc_count + 6);

    assert(length(a) == 3);

    assert(car(a).is<symbol>());
    assert(cadr(a).is<symbol>());
    assert(caddr(a).is<symbol>());
  }

  assert(default_collector::count() == gc_count + 6);
  default_collector::collect();
  assert(default_collector::count() == gc_count);

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

      assert(default_collector::count() == gc_count + 3);

      return circular_list(a, b, c);
    };

    let x = f();

    assert(car(x).as<symbol>() == "a");
    assert(cadr(x).as<symbol>() == "b");
    assert(caddr(x).as<symbol>() == "c");
    assert(cadddr(x).as<symbol>() == "a");

    default_collector::collect();

    assert(car(x).as<symbol>() == "a");
    assert(cadr(x).as<symbol>() == "b");
    assert(caddr(x).as<symbol>() == "c");
    assert(cadddr(x).as<symbol>() == "a");
  }

  default_collector::collect();

  // change class
  {
    let x = make<symbol>("hoge");

    default_collector::collect();

    assert(default_collector::count() == gc_count + 1);
    assert(x.is<symbol>());
    assert(x.as<symbol>() == "hoge");

    x = make<exact_integer>(42);

    default_collector::collect();

    assert(default_collector::count() == gc_count + 1);
    assert(x.is<exact_integer>());
    assert(x.as<exact_integer>() == 42_i64);
  }

  default_collector::collect();

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

    assert(default_collector::count() == gc_count);
    default_collector::collect();
    assert(default_collector::count() == gc_count);
  }

  default_collector::collect();

  // read list
  {
    const auto gc_count = default_collector::count();

    input_string_port("(a a a)").read();

    default_collector::collect();

    assert(default_collector::count() == gc_count + 1);
  }

  return EXIT_SUCCESS;
}
