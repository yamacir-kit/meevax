#include <vector>

#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/numerical.hpp>

namespace meevax::vector
{
  // using vector = std::vector<kernel::object>;

  struct vector
    : public std::vector<kernel::object>
  {
    template <typename... Ts>
    explicit constexpr vector(Ts&&... operands)
      : std::vector<kernel::object> {std::forward<decltype(operands)>(operands)...}
    {}
  };

  extern "C" PROCEDURE(vector_of)
  {
    vector v {};

    for (const auto& each : operands)
    {
      std::cerr << ";\t\t; " << each << std::endl;
      v.push_back(each);
    }

    return
      kernel::true_object;
      // kernel::make<vector>(v
      //   // std::begin(operands), std::end(operands)
      //   );
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
} // extern "C"

