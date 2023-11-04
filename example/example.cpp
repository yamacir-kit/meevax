#include <meevax/kernel/environment.hpp>

using namespace meevax; // NOTE: DIRTY HACK

extern "C"
{
  auto arity(object & xs)
  {
    return make<exact_integer>(length(xs));
  }

  auto dummy_procedure(object & xs)
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

  auto make_hoge(object & xs)
  {
    return make<hoge>(car(xs).as<exact_integer>());
  }

  auto is_hoge(object & xs)
  {
    return car(xs).is<hoge>() ? t : f;
  }

  auto hoge_value(object & xs)
  {
    return make<exact_integer>(car(xs).as<hoge>().value);
  }
}
