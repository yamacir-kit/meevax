#include <meevax/basis.hpp>
#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/environment.hpp>

using namespace meevax; // NOTE: DIRTY HACK

extern "C"
{
  auto argument_length(object & xs)
  {
    return make(static_cast<small_integer>(length(xs)));
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
      if (x.is<small_integer>())
      {
        std::cout << "; return incremented left-most integer object." << std::endl;

        return make(x.as<small_integer>() + 1);
      }
    }

    return unspecified;
  }

  struct hoge
  {
    small_integer value;

    ~hoge()
    {
      std::cout << "DESTRUCTOR!" << std::endl;
    }
  };

  auto make_hoge(object & xs)
  {
    return make<hoge>(exact_integer_cast<small_integer>(car(xs)));
  }

  auto is_hoge(object & xs)
  {
    return car(xs).is<hoge>() ? t : f;
  }

  auto hoge_value(object & xs)
  {
    return make<small_integer>(car(xs).as<hoge>().value);
  }
}
