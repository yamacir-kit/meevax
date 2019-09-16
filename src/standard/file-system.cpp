#include <meevax/system/boolean.hpp>
#include <meevax/system/file.hpp>
#include <meevax/system/native.hpp>
#include <meevax/system/pair.hpp>
#include <meevax/system/string.hpp>

extern "C" namespace meevax::system
{
  NATIVE(input_file_) // input-file?
  {
    return car(args).is<input_file>() ? true_object : false_object;
  }

  NATIVE(output_file_) // output-file?
  {
    return car(args).is<output_file>() ? true_object : false_object;
  }

  NATIVE(open_input_file) // open-input-file
  {
    return make<input_file>(car(args).as<string>());
  }

  NATIVE(open_output_file) // open-output-file
  {
    return make<output_file>(car(args).as<string>());
  }

  NATIVE(close_input_file) // close-input-file
  {
    car(args).as<input_file>().close();
    return unspecified;
  }

  NATIVE(close_output_file) // close-output-file
  {
    car(args).as<output_file>().close();
    return unspecified;
  }
} // extern "C"

