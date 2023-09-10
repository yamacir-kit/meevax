#undef NDEBUG

#include <cassert>
#include <meevax/kernel/environment.hpp>
#include <meevax/kernel/vector.hpp>

auto main() -> int
{
  using namespace meevax;

  const auto gc_count = gc.count();

  // make-vector
  {
    let const v = make<vector>(list(make<symbol>("a"),
                                    make<symbol>("b"),
                                    make<symbol>("c")));
    gc.collect();

    assert(v.is<vector>());
    assert(v.as<vector>().vector.size() == 3);
    assert(gc.count() == gc_count + 4);

    gc.collect();

    assert(v.is<vector>());
    assert(v.as<vector>().vector.size() == 3);
    assert(gc.count() == gc_count + 4);

    gc.collect();

    assert(v.is<vector>());
    assert(v.as<vector>().vector.size() == 3);
    assert(gc.count() == gc_count + 4);

    v.as<vector>().vector.clear();

    assert(v.is<vector>());
    assert(v.as<vector>().vector.size() == 0);
    assert(gc.count() == gc_count + 4);

    gc.collect();

    assert(v.is<vector>());
    assert(v.as<vector>().vector.size() == 0);
    assert(gc.count() == gc_count + 1);
  }

  gc.collect();

  // list->vector
  {
    let const v = make<vector>(list(make<symbol>("a"),
                                    make<symbol>("b"),
                                    make<symbol>("c")));

    assert(v.is<vector>());
    assert(v.as<vector>().vector.size() == 3);
    assert(gc.count() == gc_count + 7);

    gc.collect();

    assert(v.is<vector>());
    assert(v.as<vector>().vector.size() == 3);
    assert(gc.count() == gc_count + 4);

    gc.collect();

    assert(v.is<vector>());
    assert(v.as<vector>().vector.size() == 3);
    assert(gc.count() == gc_count + 4);

    v.as<vector>().vector.clear();

    assert(v.is<vector>());
    assert(v.as<vector>().vector.size() == 0);
    assert(gc.count() == gc_count + 4);

    gc.collect();

    assert(v.is<vector>());
    assert(v.as<vector>().vector.size() == 0);
    assert(gc.count() == gc_count + 1);
  }

  gc.collect();

  // list->vector
  {
    auto const gc_count = gc.count();

    let const v = make<vector>(input_string_port("(a b c)").read());

    assert(v.is<vector>());
    assert(v.as<vector>().vector.size() == 3);
    assert(v.as<vector>().vector[0].as<symbol>() == "a");
    assert(v.as<vector>().vector[1].as<symbol>() == "b");
    assert(v.as<vector>().vector[2].as<symbol>() == "c");

    gc.collect();

    assert(v.is<vector>());
    assert(v.as<vector>().vector.size() == 3);
    assert(v.as<vector>().vector[0].as<symbol>() == "a");
    assert(v.as<vector>().vector[1].as<symbol>() == "b");
    assert(v.as<vector>().vector[2].as<symbol>() == "c");
    assert(gc.count() == gc_count + 4);

    gc.collect();

    assert(v.is<vector>());
    assert(v.as<vector>().vector.size() == 3);
    assert(v.as<vector>().vector[0].as<symbol>() == "a");
    assert(v.as<vector>().vector[1].as<symbol>() == "b");
    assert(v.as<vector>().vector[2].as<symbol>() == "c");
    assert(gc.count() == gc_count + 4);

    v.as<vector>().vector.clear();

    assert(v.is<vector>());
    assert(v.as<vector>().vector.size() == 0);
    assert(gc.count() == gc_count + 4);

    gc.collect();

    assert(v.is<vector>());
    assert(v.as<vector>().vector.size() == 0);
  }

  symbols().clear();

  gc.collect();

  // vector literal
  {
    auto const gc_count = gc.count();

    let const v = input_string_port("#(a b c)").read();

    assert(v.is<vector>());
    assert(v.as<vector>().vector.size() == 3);

    gc.collect();

    assert(v.is<vector>());
    assert(v.as<vector>().vector.size() == 3);
    assert(gc.count() == gc_count + 4);

    gc.collect();

    assert(v.is<vector>());
    assert(v.as<vector>().vector.size() == 3);
    assert(gc.count() == gc_count + 4);

    v.as<vector>().vector.clear();

    assert(v.is<vector>());
    assert(v.as<vector>().vector.size() == 0);
    assert(gc.count() == gc_count + 4);

    gc.collect();

    assert(v.is<vector>());
    assert(v.as<vector>().vector.size() == 0);
    assert(gc.count() == gc_count + 4);
  }

  symbols().clear();

  gc.collect();

  // vector constructor
  {
    auto module = environment();

    module.define<function>("vector", [](let const& xs)
    {
      return make<vector>(xs);
    });

    module.evaluate(input_string_port("(vector 1 2 3)").read());
  }

  return EXIT_SUCCESS;
}

