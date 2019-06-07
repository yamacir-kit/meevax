#include <meevax/system/boolean.hpp>
#include <meevax/system/exception.hpp>
#include <meevax/system/pair.hpp>

namespace meevax::system
{
  const objective unit {nullptr};

  const objective unbound {make<exception>("unbound")};
  const objective undefined {make<exception>("undefined")};
  const objective unspecified {make<exception>("unspecified")};

  const objective _true_  {make<std::true_type>()};
  const objective _false_ {make<std::false_type>()};
} // namespace meevax::system


