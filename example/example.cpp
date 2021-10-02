#include <meevax/kernel/syntactic_continuation.hpp>

using namespace meevax; // NOTE: DIRTY HACK

extern "C"
{
  PROCEDURE(length_of_arguments)
  {
    return make<exact_integer>(length(xs));
  }

  PROCEDURE(dummy_procedure)
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
        return make<exact_integer>(static_cast<int>(each.as<exact_integer>()) + 1);
      }
    }

    return unspecified;
  }
}
