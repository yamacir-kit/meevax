#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/string.hpp>
#include <meevax/kernel/symbol.hpp>

extern "C" namespace meevax::symbol
{
  PROCEDURE(symbol)
  {
    if (not xs or
        not car(xs) or
        not car(xs).is<kernel::string>())
    {
      return kernel::make<kernel::symbol>();
    }
    else
    {
      return
        kernel::make<kernel::symbol>(
          car(xs).as<kernel::string>());
    }
  }

  PROCEDURE(is_symbol)
  {
    for (const auto& each : xs)
    {
      if (not each or not each.is<kernel::symbol>())
      {
        return kernel::f;
      }
    }

    return kernel::t;
  }
} // extern "C"

