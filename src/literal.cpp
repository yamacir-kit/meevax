#include <meevax/system/boolean.hpp>
#include <meevax/system/exception.hpp>
#include <meevax/system/pair.hpp>

namespace meevax::system
{
  const object unit {nullptr};

  const object unbound {make<exception>("unbound")};
  const object undefined {make<exception>("undefined")};
  const object unspecified {make<exception>("unspecified")};

  const object _true_ {make<std::true_type>()};
  const object _false_ {make<std::false_type>()};
} // namespace meevax::system


