#include <meevax/kernel/syntactic_continuation.hpp>

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

    std::size_t count { 0 };

    for (let const& each : xs)
    {
      std::cout << "; [" << count++ << "] is " << each << " (C++ typename " << each.type().name() << ")" << std::endl;
    }

    for (let const& each : xs)
    {
      if (each.is<exact_integer>())
      {
        std::cout << "; return incremented left-most integer object." << std::endl;
        return make<exact_integer>(each.as<exact_integer>().to<int>() + 1);
      }
    }

    return unspecified;
  }
}
