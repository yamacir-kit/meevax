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
} // extern "C"


