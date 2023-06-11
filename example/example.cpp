#include <meevax/kernel/environment.hpp>

using namespace meevax; // NOTE: DIRTY HACK

extern "C"
{
  let length_of_arguments(let const& xs)
  {
    return make<exact_integer>(length(xs));
  }

  let dummy_procedure(let const& xs)
  {
    std::cout << "\n; calling C++ function via foreign-function-interface." << std::endl;

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

  let make_hoge(let const& xs)
  {
    return make<hoge>(xs[0].as<exact_integer>());
  }

  let is_hoge(let const& xs)
  {
    return xs[0].is<hoge>() ? t : f;
  }

  let hoge_value(let const& xs)
  {
    return make<exact_integer>(xs[0].as<hoge>().value);
  }
}
