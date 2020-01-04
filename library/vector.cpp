#include <vector>

#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/numerical.hpp>

namespace meevax::standard
{
  // using vector = std::vector<kernel::object>;

  struct vector
    : public std::vector<kernel::object>
  {
    using std::vector<kernel::object>::vector;
  };

  extern "C" PROCEDURE(vector_of)
  {
    auto result {kernel::make<vector>()};

    for (const auto& each : operands)
    {
      std::cerr << ";\t\t; " << each << std::endl;
      result.as<vector>().push_back(each);
    }

    return result;
  }

  extern "C" PROCEDURE(vector_reference)
  {
    std::size_t index {kernel::cadr(operands).as<kernel::real>()};
    std::cerr << "; vector\t; index is " << index << std::endl;

    for (const auto& each : kernel::car(operands).as<vector>())
    {
      std::cerr << ";\t\t; " << each << std::endl;
    }

    return kernel::car(operands).as<vector>().at(index);
  }
} // namespace meevax::standard

