#include <meevax/system/boolean.hpp>
#include <meevax/system/cursor.hpp>
#include <meevax/system/exception.hpp>
#include <meevax/system/pair.hpp>

namespace meevax::system
{
  const cursor unit {nullptr};

  const cursor unbound   {make<exception>("unbound")};
  const cursor undefined {make<exception>("undefined")};

  const cursor true_v {make<std::true_type>()};
  const cursor false_v {make<std::false_type>()};
} // namespace meevax::system


