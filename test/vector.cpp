#undef NDEBUG

#include <cassert>
#include <meevax/kernel/syntactic_continuation.hpp>

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
    auto module = syntactic_continuation();

    auto const gc_count = gc.count();

    let const v = make<vector>(for_each_in, module.read("(a b c)"));

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 3);
    assert(v.as<vector>()[0].as<symbol>() == "a");
    assert(v.as<vector>()[1].as<symbol>() == "b");
    assert(v.as<vector>()[2].as<symbol>() == "c");

    gc.collect();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 3);
    assert(v.as<vector>()[0].as<symbol>() == "a");
    assert(v.as<vector>()[1].as<symbol>() == "b");
    assert(v.as<vector>()[2].as<symbol>() == "c");
    assert(gc.count() == gc_count + 4);

    gc.collect();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 3);
    assert(v.as<vector>()[0].as<symbol>() == "a");
    assert(v.as<vector>()[1].as<symbol>() == "b");
    assert(v.as<vector>()[2].as<symbol>() == "c");
    assert(gc.count() == gc_count + 4);

    v.as<vector>().clear();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 0);
    assert(gc.count() == gc_count + 4);

    gc.collect();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 0);
  }

  syntactic_continuation::symbols.clear();
  gc.collect();

  // vector literal
  {
    auto module = syntactic_continuation();

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

  syntactic_continuation::symbols.clear();
  gc.count();

  // vector constructor
  {
    auto module = syntactic_continuation();

    module.define<procedure>("vector", [](auto&&... xs)
    {
      return make<vector>(for_each_in, std::forward<decltype(xs)>(xs)...);
    });

    module.evaluate(module.read("(vector 1 2 3)"));
  }

  return EXIT_SUCCESS;
}

