#include <meevax/system/file.hpp>
#include <meevax/system/native.hpp>
#include <meevax/system/string.hpp>

extern "C" namespace meevax::io
{
  NATIVE(is_input_file)
  {
    return system::car(args).is<system::input_file>() ? system::true_object : system::false_object;
  }

  NATIVE(is_output_file)
  {
    return system::car(args).is<system::output_file>() ? system::true_object : system::false_object;
  }

  NATIVE(open_input_file)
  {
    return system::make<system::input_file>(
             system::car(args).as<system::string>()
           );
  }

  NATIVE(open_output_file)
  {
    return system::make<system::output_file>(
             system::car(args).as<system::string>()
           );
  }

  NATIVE(close_input_file)
  {
    system::car(args).as<system::input_file>().close();
    return system::unspecified;
  }

  NATIVE(close_output_file)
  {
    system::car(args).as<system::output_file>().close();
    return system::unspecified;
  }
} // extern "C"

