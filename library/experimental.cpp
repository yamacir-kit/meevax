#include <boost/cstdlib.hpp> // boost::exit_success

#include <meevax/system/boolean.hpp>
#include <meevax/system/native.hpp>
#include <meevax/system/numerical.hpp>
#include <meevax/system/string.hpp>

extern "C" namespace meevax::experimental
{
  NATIVE(emergency_exit)
  {
    if (not args or not system::car(args).is<system::real>())
    {
      std::exit(boost::exit_success);
    }
    else
    {
      // XXX DIRTY HACK
      std::exit(static_cast<int>(system::car(args).as<system::real>()));
    }

    return system::unspecified;
  }

  NATIVE(display)
  {
    for (const system::object& each : args)
    {
      if (each.is<system::string>()) // XXX DIRTY HACK
      {
        std::cout << static_cast<std::string>(each.as<system::string>());
      }
      else
      {
        std::cout << each;
      }
    }

    return system::unspecified;
  }
} // extern "C"

