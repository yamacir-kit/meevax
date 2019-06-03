#include <cstdlib> // std::exit

#include <boost/cstdlib.hpp> // boost::exit_success

#include <meevax/system/boolean.hpp>
#include <meevax/system/cursor.hpp>
#include <meevax/system/number.hpp>
#include <meevax/system/pair.hpp>
#include <meevax/system/procedure.hpp>
#include <meevax/system/srfi-1.hpp>
#include <meevax/system/string.hpp>

extern "C"
{
  using namespace meevax::system;

  PROCEDURE(addressive_equals)
  {
    using namespace meevax::system;
    return car(args) == cadr(args) ? true_v : false_v;
  }

  PROCEDURE(is_pair)
  {
    using namespace meevax::system;

    for (const auto& each : args)
    {
      if (not each or not each.is<pair>())
      {
        return false_v;
      }
    }

    return true_v;
  }


  PROCEDURE(emergency_exit)
  {
    if (not args or not car(args).template is<number>())
    {
      std::exit(boost::exit_success);
    }
    else
    {
      // XXX DIRTY HACK
      std::exit(static_cast<int>(car(args).template as<number>()));
    }

    return unit; // XXX DIRTY HACK
  }

  PROCEDURE(display)
  {
    for (const auto& each : args)
    {
      if (each.template is<string>()) // XXX DIRTY HACK
      {
        std::cout << static_cast<std::string>(each.template as<string>());
      }
      else
      {
        std::cout << each;
      }
    }

    return undefined; // XXX DIRTY HACK
  }
} // extern "C"

