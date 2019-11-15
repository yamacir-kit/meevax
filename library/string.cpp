#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/string.hpp>

extern "C" namespace meevax::string
{
  PROCEDURE(is_string)
  {
    return kernel::car(operands).is<kernel::string>() ? kernel::true_object : kernel::false_object;
  }

  PROCEDURE(character_pair)
  {
    return kernel::make<kernel::string>(kernel::car(operands), kernel::cadr(operands));
  }
} // extern "C"

