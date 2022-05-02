#undef NDEBUG

#include <cassert>
#include <meevax/kernel/environment.hpp>

auto main() -> int
{
  using namespace meevax;

  const auto gc_count = gc.count();

  // make-vector
  {
    let const v = make<vector>(make<symbol>("a"),
                               make<symbol>("b"),
                               make<symbol>("c"));

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 3);
    assert(gc.count() == gc_count + 4);

    gc.collect();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 3);
    assert(gc.count() == gc_count + 4);

    gc.collect();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 3);
    assert(gc.count() == gc_count + 4);

    v.as<vector>().clear();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 0);
    assert(gc.count() == gc_count + 4);

    gc.collect();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 0);
    assert(gc.count() == gc_count + 1);
  }

  gc.collect();

  // list->vector
  {
    let const v = make<vector>(for_each_in, list(make<symbol>("a"),
                                                 make<symbol>("b"),
                                                 make<symbol>("c")));

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 3);
    assert(gc.count() == gc_count + 7);

    gc.collect();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 3);
    assert(gc.count() == gc_count + 4);

    gc.collect();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 3);
    assert(gc.count() == gc_count + 4);

    v.as<vector>().clear();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 0);
    assert(gc.count() == gc_count + 4);

    gc.collect();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 0);
    assert(gc.count() == gc_count + 1);
  }

  gc.collect();

  // list->vector
  {
    auto module = environment();

    auto const gc_count = gc.count();

    let const v = make<vector>(for_each_in, module.read("(a b c)"));

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 3);
    assert(v.as<vector>()[0].as<symbol>().value == "a");
    assert(v.as<vector>()[1].as<symbol>().value == "b");
    assert(v.as<vector>()[2].as<symbol>().value == "c");

    gc.collect();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 3);
    assert(v.as<vector>()[0].as<symbol>().value == "a");
    assert(v.as<vector>()[1].as<symbol>().value == "b");
    assert(v.as<vector>()[2].as<symbol>().value == "c");
    assert(gc.count() == gc_count + 4);

    gc.collect();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 3);
    assert(v.as<vector>()[0].as<symbol>().value == "a");
    assert(v.as<vector>()[1].as<symbol>().value == "b");
    assert(v.as<vector>()[2].as<symbol>().value == "c");
    assert(gc.count() == gc_count + 4);

    v.as<vector>().clear();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 0);
    assert(gc.count() == gc_count + 4);

    gc.collect();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 0);
  }

  environment::symbols.clear();
  gc.collect();

  // vector literal
  {
    auto module = environment();

    auto const gc_count = gc.count();

    let const v = module.read("#(a b c)");

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 3);

    gc.collect();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 3);
    assert(gc.count() == gc_count + 4);

    gc.collect();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 3);
    assert(gc.count() == gc_count + 4);

    v.as<vector>().clear();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 0);
    assert(gc.count() == gc_count + 4);

    gc.collect();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 0);
    assert(gc.count() == gc_count + 4);
  }

  environment::symbols.clear();
  gc.count();

  // vector constructor
  {
    auto module = environment();

    module.define<procedure>("vector", [](let const& xs)
    {
      return make<vector>(for_each_in, xs);
    });

    module.evaluate(module.read("(vector 1 2 3)"));
  }

  return EXIT_SUCCESS;
}

