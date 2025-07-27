#undef NDEBUG

#include <cassert>
#include <meevax/kernel/environment.hpp>
#include <meevax/kernel/input_string_port.hpp>
#include <meevax/kernel/vector.hpp>
#include <meevax/kernel/procedure.hpp>

auto main() -> int
{
  using namespace meevax;

  const auto gc_count = default_collector::count();

  // make-vector
  {
    let const v = make_vector(list(make<symbol>("a"),
                                   make<symbol>("b"),
                                   make<symbol>("c")));
    default_collector::collect();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 3);
    assert(default_collector::count() == gc_count + 4);

    default_collector::collect();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 3);
    assert(default_collector::count() == gc_count + 4);

    default_collector::collect();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 3);
    assert(default_collector::count() == gc_count + 4);

    v.as<vector>().clear();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 0);
    assert(default_collector::count() == gc_count + 4);

    default_collector::collect();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 0);
    assert(default_collector::count() == gc_count + 1);
  }

  default_collector::collect();

  // list->vector
  {
    let const v = make_vector(list(make<symbol>("a"),
                                   make<symbol>("b"),
                                   make<symbol>("c")));

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 3);
    assert(default_collector::count() == gc_count + 7);

    default_collector::collect();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 3);
    assert(default_collector::count() == gc_count + 4);

    default_collector::collect();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 3);
    assert(default_collector::count() == gc_count + 4);

    v.as<vector>().clear();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 0);
    assert(default_collector::count() == gc_count + 4);

    default_collector::collect();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 0);
    assert(default_collector::count() == gc_count + 1);
  }

  default_collector::collect();

  // list->vector
  {
    auto const gc_count = default_collector::count();

    let const v = make_vector(input_string_port("(a b c)").read());

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 3);
    assert(v.as<vector>()[0].as<symbol>() == "a");
    assert(v.as<vector>()[1].as<symbol>() == "b");
    assert(v.as<vector>()[2].as<symbol>() == "c");

    default_collector::collect();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 3);
    assert(v.as<vector>()[0].as<symbol>() == "a");
    assert(v.as<vector>()[1].as<symbol>() == "b");
    assert(v.as<vector>()[2].as<symbol>() == "c");
    assert(default_collector::count() == gc_count + 4);

    default_collector::collect();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 3);
    assert(v.as<vector>()[0].as<symbol>() == "a");
    assert(v.as<vector>()[1].as<symbol>() == "b");
    assert(v.as<vector>()[2].as<symbol>() == "c");
    assert(default_collector::count() == gc_count + 4);

    v.as<vector>().clear();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 0);
    assert(default_collector::count() == gc_count + 4);

    default_collector::collect();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 0);
  }

  symbols().clear();

  default_collector::collect();

  // vector literal
  {
    auto const gc_count = default_collector::count();

    let const v = input_string_port("#(a b c)").read();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 3);

    default_collector::collect();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 3);
    assert(default_collector::count() == gc_count + 4);

    default_collector::collect();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 3);
    assert(default_collector::count() == gc_count + 4);

    v.as<vector>().clear();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 0);
    assert(default_collector::count() == gc_count + 4);

    default_collector::collect();

    assert(v.is<vector>());
    assert(v.as<vector>().size() == 0);
    assert(default_collector::count() == gc_count + 4);
  }

  symbols().clear();

  default_collector::collect();

  // vector constructor
  {
    auto module = environment();

    module.define<procedure>("vector", [](let const& xs)
    {
      return make<vector>(xs.begin(), xs.end());
    });

    module.evaluate(input_string_port("(vector 1 2 3)").read());
  }

  return EXIT_SUCCESS;
}

