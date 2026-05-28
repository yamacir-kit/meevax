#undef NDEBUG

#include <cassert>
#include <meevax/kernel/environment.hpp>
#include <meevax/kernel/input_string_port.hpp>
#include <meevax/kernel/vector.hpp>
#include <meevax/kernel/procedure.hpp>

auto main() -> int
{
  using namespace meevax;

  const auto gc_count = count();

  // make-vector
  {
    let const v = make_vector_from_list(list(make<symbol>("a"),
                                             make<symbol>("b"),
                                             make<symbol>("c")));
    collect();

    assert(v.is<vector>());
    assert(v.as<vector>().objects.size() == 3);
    assert(count() == gc_count + 4);

    collect();

    assert(v.is<vector>());
    assert(v.as<vector>().objects.size() == 3);
    assert(count() == gc_count + 4);

    collect();

    assert(v.is<vector>());
    assert(v.as<vector>().objects.size() == 3);
    assert(count() == gc_count + 4);

    v.as<vector>().objects.clear();

    assert(v.is<vector>());
    assert(v.as<vector>().objects.size() == 0);
    assert(count() == gc_count + 4);

    collect();

    assert(v.is<vector>());
    assert(v.as<vector>().objects.size() == 0);
    assert(count() == gc_count + 1);
  }

  collect();

  // list->vector
  {
    let const v = make_vector_from_list(list(make<symbol>("a"),
                                             make<symbol>("b"),
                                             make<symbol>("c")));

    assert(v.is<vector>());
    assert(v.as<vector>().objects.size() == 3);
    assert(count() == gc_count + 7);

    collect();

    assert(v.is<vector>());
    assert(v.as<vector>().objects.size() == 3);
    assert(count() == gc_count + 4);

    collect();

    assert(v.is<vector>());
    assert(v.as<vector>().objects.size() == 3);
    assert(count() == gc_count + 4);

    v.as<vector>().objects.clear();

    assert(v.is<vector>());
    assert(v.as<vector>().objects.size() == 0);
    assert(count() == gc_count + 4);

    collect();

    assert(v.is<vector>());
    assert(v.as<vector>().objects.size() == 0);
    assert(count() == gc_count + 1);
  }

  collect();

  // list->vector
  {
    auto const gc_count = count();

    let const v = make_vector_from_list(input_string_port("(a b c)").read());

    assert(v.is<vector>());
    assert(v.as<vector>().objects.size() == 3);
    assert(v.as<vector>().objects[0].as<symbol>().name == "a");
    assert(v.as<vector>().objects[1].as<symbol>().name == "b");
    assert(v.as<vector>().objects[2].as<symbol>().name == "c");

    collect();

    assert(v.is<vector>());
    assert(v.as<vector>().objects.size() == 3);
    assert(v.as<vector>().objects[0].as<symbol>().name == "a");
    assert(v.as<vector>().objects[1].as<symbol>().name == "b");
    assert(v.as<vector>().objects[2].as<symbol>().name == "c");
    assert(count() == gc_count + 4);

    collect();

    assert(v.is<vector>());
    assert(v.as<vector>().objects.size() == 3);
    assert(v.as<vector>().objects[0].as<symbol>().name == "a");
    assert(v.as<vector>().objects[1].as<symbol>().name == "b");
    assert(v.as<vector>().objects[2].as<symbol>().name == "c");
    assert(count() == gc_count + 4);

    v.as<vector>().objects.clear();

    assert(v.is<vector>());
    assert(v.as<vector>().objects.size() == 0);
    assert(count() == gc_count + 4);

    collect();

    assert(v.is<vector>());
    assert(v.as<vector>().objects.size() == 0);
  }

  symbols().clear();

  collect();

  // vector literal
  {
    auto const gc_count = count();

    let const v = input_string_port("#(a b c)").read();

    assert(v.is<vector>());
    assert(v.as<vector>().objects.size() == 3);

    collect();

    assert(v.is<vector>());
    assert(v.as<vector>().objects.size() == 3);
    assert(count() == gc_count + 4);

    collect();

    assert(v.is<vector>());
    assert(v.as<vector>().objects.size() == 3);
    assert(count() == gc_count + 4);

    v.as<vector>().objects.clear();

    assert(v.is<vector>());
    assert(v.as<vector>().objects.size() == 0);
    assert(count() == gc_count + 4);

    collect();

    assert(v.is<vector>());
    assert(v.as<vector>().objects.size() == 0);
    assert(count() == gc_count + 4);
  }

  symbols().clear();

  collect();

  // vector constructor
  {
    auto module = environment();

    module.define(make_symbol("vector"), make<procedure>("vector", [](let const& xs)
    {
      return make_vector_from_list(xs);
    }));

    module.evaluate(input_string_port("(vector 1 2 3)").read());
  }

  return EXIT_SUCCESS;
}

