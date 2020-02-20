#include <meevax/kernel/port.hpp>
#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/string.hpp>

extern "C" namespace meevax::io
{
  PROCEDURE(is_input_port)
  {
    return kernel::car(operands).is<kernel::input_port>() ? kernel::true_object : kernel::false_object;
  }

  PROCEDURE(is_output_port)
  {
    return kernel::car(operands).is<kernel::output_port>() ? kernel::true_object : kernel::false_object;
  }

  PROCEDURE(open_input_file)
  {
    return kernel::make<kernel::input_port>(
             kernel::car(operands).as<kernel::string>()
           );
  }

  PROCEDURE(open_output_file)
  {
    return
      kernel::make<kernel::output_port>(
        kernel::car(operands).as<kernel::string>());
  }

  PROCEDURE(close_input_port)
  {
    kernel::car(operands).as<kernel::input_port>().close();
    return kernel::unspecified;
  }

  PROCEDURE(close_output_port)
  {
    kernel::car(operands).as<kernel::output_port>().close();
    return kernel::unspecified;
  }
} // extern "C"

