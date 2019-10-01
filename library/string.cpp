#include <meevax/kernel/native.hpp>
#include <meevax/kernel/string.hpp>

extern "C" namespace meevax::string
{
  NATIVE(is_string)
  {
    return kernel::car(operands).is<kernel::string>() ? kernel::true_object : kernel::false_object;
  }

  NATIVE(character_pair)
  {
    return kernel::make<kernel::string>(kernel::car(operands), kernel::cadr(operands));
  }
} // extern "C"

