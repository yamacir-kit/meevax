#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/string.hpp>
#include <meevax/kernel/symbol.hpp>

extern "C" namespace meevax::symbol
{
  PROCEDURE(symbol)
  {
    if (not operands or
        not car(operands) or
        not car(operands).is<kernel::string>())
    {
      return kernel::make<kernel::symbol>();
    }
    else
    {
      return
        kernel::make<kernel::symbol>(
          car(operands).as<kernel::string>());
    }
  }

  PROCEDURE(is_symbol)
  {
    for (const auto& each : operands)
    {
      if (not each or not each.is<kernel::symbol>())
      {
        return kernel::false_object;
      }
    }

    return kernel::true_object;
  }
} // extern "C"

