#include <meevax/kernel/character.hpp>
#include <meevax/kernel/numerical.hpp>
#include <meevax/kernel/procedure.hpp>

extern "C" namespace meevax::character
{
  PROCEDURE(is_character)
  {
    return kernel::convert(kernel::car(xs).template is<kernel::character>());
  }

  PROCEDURE(digit_value)
  {
    // XXX INCORRECT!!!
    return
      kernel::make<kernel::real>(
        kernel::car(xs).as<const std::string>());
  }

  PROCEDURE(codepoint)
  {
    using namespace meevax::kernel;

    switch (const std::string& s { car(xs).as<const std::string>() }; s.size())
    {
    case 1:
      return make<real>(*reinterpret_cast<const std::uint8_t*>(s.data()));

    default:
      throw make<evaluation_error>("unicode unsupported");
    }
  }
} // extern "C"

