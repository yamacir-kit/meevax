#include <meevax/kernel/native.hpp>
#include <meevax/kernel/character.hpp>

extern "C" namespace meevax::string
{
  NATIVE(is_character)
  {
    return kernel::car(operands).is<kernel::character>() ? kernel::true_object : kernel::false_object;
  }
} // extern "C"

