#include <boost/cstdlib.hpp> // boost::exit_success

#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/native.hpp>
#include <meevax/kernel/numerical.hpp>
#include <meevax/kernel/string.hpp>

extern "C" namespace meevax::experimental
{
  NATIVE(emergency_exit)
  {
    if (not args or not kernel::car(args).is<kernel::real>())
    {
      std::exit(boost::exit_success);
    }
    else
    {
      // XXX DIRTY HACK
      std::exit(static_cast<int>(kernel::car(args).as<kernel::real>()));
    }

    return kernel::unspecified;
  }

  NATIVE(display)
  {
    for (const kernel::object& each : args)
    {
      if (each.is<kernel::string>()) // XXX DIRTY HACK
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

