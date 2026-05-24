#undef NDEBUG

#include <cassert>
#include <meevax/kernel/environment.hpp>
#include <meevax/kernel/input_string_port.hpp>
#include <meevax/kernel/proper_list.hpp>
#include <meevax/kernel/symbol.hpp>

auto main() -> int
{
  using namespace meevax;

  const auto gc_count = count();

  // scope
  {
    {
      let x = make<symbol>("x");
      let y = make<symbol>("y");
      let z = make<symbol>("z");

      assert(count() == gc_count + 3);
      collect();
      assert(count() == gc_count + 3);
    }

    assert(count() == gc_count + 3);
    collect();
    assert(count() == gc_count);
  }

  assert(count() == gc_count);
  collect();
  assert(count() == gc_count);

  // copy
  {
    let x = make<symbol>("hoge");
    let y = x;

    assert(x.is<symbol>());
    assert(y.is<symbol>());

    assert(eq(x, y));
    assert(eqv(x, y));
  }

  assert(count() == gc_count + 1);
  collect();
  assert(count() == gc_count);

  // move
  {
    auto f = []()
    {
      let x = make<symbol>("x");

      assert(x.is<symbol>());
      assert(x.as<symbol>().name == "x");

      return x; // return value optimization
    };

    let x = f();

    assert(x.is<symbol>());
    assert(x.as<symbol>().name == "x");

    assert(count() == gc_count + 1);
    collect();
    assert(count() == gc_count + 1);

    assert(x.is<symbol>());
    assert(x.as<symbol>().name == "x");
  }

  assert(count() == gc_count + 1);
  collect();
  assert(count() == gc_count);

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

      assert(x.as<symbol>().name == "x");
      assert(y.as<symbol>().name == "y");
      assert(z.as<symbol>().name == "z");

      assert(count() == gc_count + 3);
      collect();
      assert(count() == gc_count + 3);

      return list(x, y, z);
    };

    let a = f();

    assert(length(a) == 3);

    assert(car(a).is<symbol>());
    assert(cadr(a).is<symbol>());
    assert(caddr(a).is<symbol>());

    assert(count() == gc_count + 6);
    collect();
    assert(count() == gc_count + 6);

    assert(length(a) == 3);

    assert(car(a).is<symbol>());
    assert(cadr(a).is<symbol>());
    assert(caddr(a).is<symbol>());
  }

  assert(count() == gc_count + 6);
  collect();
  assert(count() == gc_count);

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

      assert(a.as<symbol>().name == "a");
      assert(b.as<symbol>().name == "b");
      assert(c.as<symbol>().name == "c");

      assert(count() == gc_count + 3);

      return circular_list(a, b, c);
    };

    let x = f();

    assert(car(x).as<symbol>().name == "a");
    assert(cadr(x).as<symbol>().name == "b");
    assert(caddr(x).as<symbol>().name == "c");
    assert(cadddr(x).as<symbol>().name == "a");

    collect();

    assert(car(x).as<symbol>().name == "a");
    assert(cadr(x).as<symbol>().name == "b");
    assert(caddr(x).as<symbol>().name == "c");
    assert(cadddr(x).as<symbol>().name == "a");
  }

  collect();

  // change class
  {
    let x = make<symbol>("hoge");

    collect();

    assert(count() == gc_count + 1);
    assert(x.is<symbol>());
    assert(x.as<symbol>().name == "hoge");

    x = make<large_integer>(42);

    collect();

    assert(count() == gc_count + 1);
    assert(x.is<large_integer>());
    assert(x.as<large_integer>() == 42_i64);
  }

  collect();

  // number
  {
    {
      let const x = make<double>(42.0);

      assert(x.is<double>());
      assert(x.as<double>() == 42);

      let const y = make<double>(std::stod("42"));

      assert(y.is<double>());
      assert(y.as<double>() == 42);
    }

    assert(count() == gc_count);
    collect();
    assert(count() == gc_count);
  }

  collect();

  // read list
  {
    const auto gc_count = count();

    input_string_port("(a a a)").read();

    collect();

    assert(count() == gc_count + 1);
  }

  return EXIT_SUCCESS;
}
