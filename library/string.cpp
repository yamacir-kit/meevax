#include <meevax/kernel/numerical.hpp>
#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/reader.hpp>
#include <meevax/kernel/string.hpp>

extern "C" namespace meevax::string
{
  PROCEDURE(is_string)
  {
    return MEEVAX_API_TYPE_PREDICATE(kernel::string);
  }

  PROCEDURE(ccons)
  {
    return
      kernel::make<kernel::string>(
        kernel::car(operands),
        kernel::cadr(operands));
  }

  PROCEDURE(string_from_number)
  {
    const auto number {kernel::car(operands).as<kernel::real>().str()};

    return
      kernel::datum<kernel::string>(number);
  }
} // extern "C"

