#include <meevax/kernel/environment.hpp>

using namespace meevax; // NOTE: DIRTY HACK

extern "C"
{
  auto length_of_arguments(object const& xs)
  {
    return make<exact_integer>(length(xs));
  }

  auto dummy_procedure(object const& xs)
  {
    std::cout << "\n; calling C++ function." << std::endl;

    std::size_t count = 0;

    for (let const& each : xs)
    {
      std::cout << "; [" << count++ << "] is " << each << " (C++ typename " << each.type().name() << ")" << std::endl;
    }

    for (let const& x : xs)
    {
      if (x.is<exact_integer>())
      {
        std::cout << "; return incremented left-most integer object." << std::endl;

        auto value = static_cast<int>(x.as<exact_integer>());

        return make<exact_integer>(++value);
      }
    }

    return unspecified;
  }

  struct hoge
  {
    int value;

    ~hoge()
    {
      std::cout << "DESTRUCTOR!" << std::endl;
    }
  };

  auto make_hoge(object const& xs)
  {
    return make<hoge>(xs[0].as<exact_integer>());
  }

  auto is_hoge(object const& xs)
  {
    return xs[0].is<hoge>() ? t : f;
  }

  auto hoge_value(object const& xs)
  {
    return make<exact_integer>(xs[0].as<hoge>().value);
  }
}
