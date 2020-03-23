#include <meevax/kernel/port.hpp>
#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/string.hpp>

extern "C" namespace meevax::kernel
{
  PROCEDURE(is_input_port)
  {
    return
      convert(
        car(operands).is<input_port>());
  }

  PROCEDURE(is_output_port)
  {
    return
      convert(
        car(operands).is<output_port>());
  }

  PROCEDURE(open_input_file)
  {
    return
      make<input_port>(
        car(operands).as<string>());
  }

  PROCEDURE(open_output_file)
  {
    return
      make<output_port>(
        car(operands).as<string>());
  }

  PROCEDURE(close_input_port)
  {
    car(operands).as<input_port>().close();
    return unspecified;
  }

  PROCEDURE(close_output_port)
  {
    car(operands).as<output_port>().close();
    return unspecified;
  }
} // extern "C"

