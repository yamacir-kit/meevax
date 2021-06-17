#include <meevax/kernel/ghost.hpp>
#include <meevax/kernel/number.hpp>
#include <meevax/kernel/procedure.hpp>

extern "C"
{
  using namespace meevax::kernel;

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
        return make<exact_integer>(each.as<exact_integer>().to<int>() + 1);
      }
    }

    return unspecified;
  }
} // extern "C"
