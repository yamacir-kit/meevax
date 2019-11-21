#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/string.hpp>
#include <meevax/kernel/symbol.hpp>

extern "C" namespace meevax::symbol
{
  PROCEDURE(symbol)
  {
    try
    {
      return kernel::make<kernel::symbol>(
               car(operands).as<kernel::string>()
             );
    }
    catch (...) // XXX DIRTY HACK
    {
      return kernel::make<kernel::symbol>();
    }
  }

  PROCEDURE(is_symbol)
  {
    for (const auto& each : operands)
    {
      if (not each or not each.is<kernel::pair>())
      {
        return kernel::false_object;
      }
    }

    return kernel::true_object;
  }
} // extern "C"


