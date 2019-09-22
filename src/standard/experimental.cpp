#include <boost/cstdlib.hpp> // boost::exit_success

#include <meevax/system/boolean.hpp>
#include <meevax/system/native.hpp>
#include <meevax/system/numerical.hpp>
#include <meevax/system/string.hpp>

extern "C" namespace meevax::system
{
  NATIVE(eq_)
  {
    return car(args) == cadr(args) ? true_object : false_object;
  }

  NATIVE(eqv_)
  {
    if (const object& object1 {car(args)}, object2 {cadr(args)}; object1 == object2)
    {
      return true_object;
    }
    else if (!object1 or !object2)
    {
      return false_object;
    }
    else
    {
      return object1.equals(object2) ? true_object : false_object;
    }
  }

  NATIVE(emergency_exit)
  {
    if (not args or not car(args).is<real>())
    {
      std::exit(boost::exit_success);
    }
    else
    {
      // XXX DIRTY HACK
      std::exit(static_cast<int>(car(args).as<real>()));
    }

    return unspecified;
  }

  NATIVE(display)
  {
    for (const object& each : args)
    {
      if (each.is<string>()) // XXX DIRTY HACK
      {
        std::cout << static_cast<std::string>(each.as<string>());
      }
      else
      {
        std::cout << each;
      }
    }

    return unspecified;
  }
} // extern "C"

