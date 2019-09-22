#include <meevax/system/native.hpp>
#include <meevax/system/string.hpp>
#include <meevax/system/symbol.hpp>

extern "C"
{
  NATIVE(symbol)
  {
    try
    {
      return meevax::system::make<meevax::system::symbol>(
               car(args).as<meevax::system::string>()
             );
    }
    catch (...) // XXX DIRTY HACK
    {
      return meevax::system::make<meevax::system::symbol>();
    }
  }

  NATIVE(is_symbol)
  {
    for (const auto& each : args)
    {
      if (not each or not each.is<meevax::system::pair>())
      {
        return meevax::system::false_object;
      }
    }

    return meevax::system::true_object;
  }
} // extern "C"


