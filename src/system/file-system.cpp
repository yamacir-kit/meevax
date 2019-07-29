#include <meevax/system/boolean.hpp>
#include <meevax/system/file.hpp>
#include <meevax/system/pair.hpp>
#include <meevax/system/procedure.hpp>
#include <meevax/system/srfi-1.hpp>
#include <meevax/system/string.hpp>

extern "C"
{
  PROCEDURE(input_file_) // input-file?
  {
    using namespace meevax::system;
    return car(args).is<input_file>() ? _true_ : _false_;
  }

  PROCEDURE(output_file_) // output-file?
  {
    using namespace meevax::system;
    return car(args).is<output_file>() ? _true_ : _false_;
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
} // extern "C"

