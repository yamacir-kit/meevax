#include <meevax/system/boolean.hpp>
#include <meevax/system/file.hpp>
#include <meevax/system/list.hpp>
#include <meevax/system/pair.hpp>
#include <meevax/system/procedure.hpp>
#include <meevax/system/string.hpp>

extern "C"
{
  PROCEDURE(input_file_) // input-file?
  {
    using namespace meevax::system;
    return car(args).is<input_file>() ? true_object : false_object;
  }

  PROCEDURE(output_file_) // output-file?
  {
    using namespace meevax::system;
    return car(args).is<output_file>() ? true_object : false_object;
  }

  PROCEDURE(open_input_file) // open-input-file
  {
    using namespace meevax::system;
    return make<input_file>(car(args).as<string>());
  }

  PROCEDURE(open_output_file) // open-output-file
  {
    using namespace meevax::system;
    return make<output_file>(car(args).as<string>());
  }

  PROCEDURE(close_input_file) // close-input-file
  {
    using namespace meevax::system;
    car(args).as<input_file>().close();
    return unspecified;
  }

  PROCEDURE(close_output_file) // close-output-file
  {
    using namespace meevax::system;
    car(args).as<output_file>().close();
    return unspecified;
  }
} // extern "C"

