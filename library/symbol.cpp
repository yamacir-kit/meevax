#include <meevax/kernel/native.hpp>
#include <meevax/kernel/string.hpp>
#include <meevax/kernel/symbol.hpp>

extern "C" namespace meevax::symbol
{
  NATIVE(symbol)
  {
    try
    {
      return kernel::make<kernel::symbol>(
               car(args).as<kernel::string>()
             );
    }
    catch (...) // XXX DIRTY HACK
    {
      return kernel::make<kernel::symbol>();
    }
  }

  NATIVE(is_symbol)
  {
    for (const auto& each : args)
    {
      if (not each or not each.is<kernel::pair>())
      {
        return kernel::false_object;
      }
    }

    return kernel::true_object;
  }
} // extern "C"


