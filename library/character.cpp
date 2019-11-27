#include <meevax/kernel/character.hpp>
#include <meevax/kernel/numerical.hpp>
#include <meevax/kernel/procedure.hpp>

extern "C" namespace meevax::character
{
  PROCEDURE(is_character)
  {
    return
      MEEVAX_BOOLEAN(
        kernel::car(operands).is<kernel::character>());
  }

  PROCEDURE(digit_value)
  {
    // XXX INCORRECT!!!
    return
      kernel::make<kernel::real>(
        kernel::car(operands).as<const std::string>());
  }

  PROCEDURE(codepoint)
  {
    switch (const std::string& s {
              kernel::car(operands).as<const std::string>()
            }; s.size())
    {
    case 1:
      return
        kernel::allocate<kernel::real>(
          resource,
          *reinterpret_cast<const std::uint8_t*>(s.data()));

    default:
      throw kernel::make<kernel::evaluation_error>("unicode unsupported");
    }
  }
} // extern "C"

