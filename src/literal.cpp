#include <meevax/system/boolean.hpp>
#include <meevax/system/exception.hpp>
#include <meevax/system/pair.hpp>

namespace meevax::system
{
  const object unit {nullptr};

  const object unbound {make<exception>("unbound")};
  const object undefined {make<exception>("undefined")};
  const object unspecified {make<exception>("unspecified")};

  const object true_object {make<std::true_type>()};
  const object false_object {make<std::false_type>()};
} // namespace meevax::system


