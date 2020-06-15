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
    return kernel::make<kernel::string>(kernel::car(xs), kernel::cadr(xs));
  }

  PROCEDURE(string_from_number)
  {
    using namespace kernel;

    return read_string(car(xs).as<real>().str());
  }
} // extern "C"

