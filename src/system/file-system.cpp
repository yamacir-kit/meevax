#include <meevax/system/boolean.hpp>
#include <meevax/system/file.hpp>
#include <meevax/system/pair.hpp>
#include <meevax/system/procedure.hpp>
#include <meevax/system/srfi-1.hpp>

extern "C"
{
  PROCEDURE(input_file_)
  {
    using namespace meevax::system;
    return car(args).is<input_file>() ? _true_ : _false_;
  }

  PROCEDURE(output_file_)
  {
    using namespace meevax::system;
    return car(args).is<output_file>() ? _true_ : _false_;
  }
} // extern "C"

