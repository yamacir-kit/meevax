#include <boost/cstdlib.hpp> // boost::exit_success

#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/numerical.hpp>
#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/string.hpp>

extern "C" namespace meevax::experimental
{
  PROCEDURE(emergency_exit)
  {
    if (not operands or not kernel::car(operands).is<kernel::real>())
    {
      std::exit(boost::exit_success);
    }
    else
    {
      // XXX DIRTY HACK
      std::exit(static_cast<int>(kernel::car(operands).as<kernel::real>()));
    }

    return kernel::unspecified;
  }

  PROCEDURE(display)
  {
    for (const kernel::object& each : operands)
    {
      if (each and each.is<kernel::string>()) // XXX DIRTY HACK
      {
        std::cout << static_cast<std::string>(each.as<kernel::string>());
      }
      else
      {
        std::cout << each;
      }
    }

    return kernel::unspecified;
  }
} // extern "C"

