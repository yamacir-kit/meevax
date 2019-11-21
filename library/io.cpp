#include <meevax/kernel/file.hpp>
#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/string.hpp>

extern "C" namespace meevax::io
{
  PROCEDURE(is_input_file)
  {
    return kernel::car(operands).is<kernel::input_file>() ? kernel::true_object : kernel::false_object;
  }

  PROCEDURE(is_output_file)
  {
    return kernel::car(operands).is<kernel::output_file>() ? kernel::true_object : kernel::false_object;
  }

  PROCEDURE(open_input_file)
  {
    return kernel::make<kernel::input_file>(
             kernel::car(operands).as<kernel::string>()
           );
  }

  PROCEDURE(open_output_file)
  {
    return
      kernel::make<kernel::output_file>(
        kernel::car(operands).as<kernel::string>());
  }

  PROCEDURE(close_input_file)
  {
    kernel::car(operands).as<kernel::input_file>().close();
    return kernel::unspecified;
  }

  PROCEDURE(close_output_file)
  {
    kernel::car(operands).as<kernel::output_file>().close();
    return kernel::unspecified;
  }
} // extern "C"

