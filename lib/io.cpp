#include <meevax/kernel/port.hpp>
#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/string.hpp>

extern "C" namespace meevax { inline namespace kernel
{
  PROCEDURE(is_input_port)
  {
    return convert(car(xs).is<input_port>());
  }

  PROCEDURE(is_output_port)
  {
    return convert(car(xs).is<output_port>());
  }

  PROCEDURE(open_input_file)
  {
    return make<input_port>(car(xs).as<string>());
  }

  PROCEDURE(open_output_file)
  {
    return make<output_port>(car(xs).as<string>());
  }

  PROCEDURE(close_input_port)
  {
    car(xs).as<input_port>().close();
    return unspecified;
  }

  PROCEDURE(close_output_port)
  {
    car(xs).as<output_port>().close();
    return unspecified;
  }
}} // extern "C"
