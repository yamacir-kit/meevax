#include <meevax/kernel/path.hpp>
#include <meevax/posix/vt102.hpp>

namespace meevax { inline namespace kernel
{
  auto operator <<(std::ostream& port, const path& datum) -> decltype(port)
  {
    return port << cyan << "#p\"" << datum.c_str() << "\"" << reset;
  }
}} // namespace meevax::kernel
