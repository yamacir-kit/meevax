#include <meevax/kernel/character.hpp>
#include <meevax/kernel/native.hpp>
#include <meevax/kernel/numerical.hpp>

extern "C" namespace meevax::character
{
  NATIVE(is_character)
  {
    return kernel::car(operands).is<kernel::character>() ? kernel::true_object : kernel::false_object;
  }

  NATIVE(digit_value)
  {
    // XXX INCORRECT!!!
    return kernel::make<kernel::real>(
             kernel::car(operands).as<const std::string>()
           );
  }

  NATIVE(codepoint)
  {
    switch (const std::string& s {
              kernel::car(operands).as<const std::string&>()
            }; s.size())
    {
    case 1:
      return kernel::make<kernel::real>(
               *reinterpret_cast<const std::uint8_t*>(s.data())
             );

    default:
      throw kernel::make<kernel::evaluation_error>(
              "character_to_real - unsupported"
            );
    }
  }
} // extern "C"

