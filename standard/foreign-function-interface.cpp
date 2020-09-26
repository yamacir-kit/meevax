#include <meevax/kernel/numerical.hpp>
#include <meevax/kernel/procedure.hpp>

extern "C"
{
  using namespace meevax::kernel;

  PROCEDURE(dummy_procedure)
  {
    std::cout << "\n; calling C++ function via foreign-function-interface." << std::endl;

    std::size_t count { 0 };

    for (const object& each : xs)
    {
      std::cout << "; [" << count++ << "] is " << each << " (C++ typename " << each.type().name() << ")" << std::endl;
    }

    for (const object& each : xs)
    {
      if (each.is<exact_integer>())
      {
        std::cout << "; return incremented left-most integer object." << std::endl;
        return make<exact_integer>(each.as<exact_integer>().value.convert_to<int>() + 1);
      }
    }

    return unspecified;
  }
} // extern "C"
