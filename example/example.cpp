#include <meevax/library/standard.hpp>

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
}
