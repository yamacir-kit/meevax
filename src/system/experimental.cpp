#include <meevax/system/boolean.hpp>
#include <meevax/system/cursor.hpp>
#include <meevax/system/pair.hpp>
#include <meevax/system/procedure.hpp>
#include <meevax/system/srfi-1.hpp>

extern "C"
{
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
} // extern "C"

