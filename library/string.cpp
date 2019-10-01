#include <meevax/kernel/native.hpp>
#include <meevax/kernel/string.hpp>

extern "C" namespace meevax::string
{
  NATIVE(is_string)
  {
    return kernel::car(operands).is<kernel::string>() ? kernel::true_object : kernel::false_object;
  }
} // extern "C"

