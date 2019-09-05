#include <cstdlib> // std::exit

#include <boost/cstdlib.hpp> // boost::exit_success

#include <meevax/system/boolean.hpp>
#include <meevax/system/list.hpp>
#include <meevax/system/numerical.hpp>
#include <meevax/system/pair.hpp>
#include <meevax/system/procedure.hpp>
#include <meevax/system/string.hpp>

extern "C"
{
  PROCEDURE(eq_)
  {
    using namespace meevax::system;
    return car(args) == cadr(args) ? true_object : false_object;
  }

  PROCEDURE(eqv_)
  {
    using namespace meevax::system;

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

  PROCEDURE(pair_)
  {
    using namespace meevax::system;

    for (const auto& each : args)
    {
      if (not each or not each.is<pair>())
      {
        return false_object;
      }
    }

    return true_object;
  }


  PROCEDURE(emergency_exit)
  {
    using namespace meevax::system;

    if (not args or not car(args).is<real>())
    {
      std::exit(boost::exit_success);
    }
    else
    {
      // XXX DIRTY HACK
      std::exit(static_cast<int>(car(args).as<real>()));
    }

    return unit; // XXX DIRTY HACK
  }

  PROCEDURE(display)
  {
    using namespace meevax::system;

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

    return unspecified; // XXX DIRTY HACK
  }
} // extern "C"

