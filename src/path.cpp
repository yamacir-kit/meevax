#include <meevax/kernel/path.hpp>
#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace kernel
{
  auto operator <<(std::ostream & port, path const& datum) -> std::ostream &
  {
    return port << cyan << "#p\"" << datum.c_str() << "\"" << reset;
  }
} // namespace kernel
} // namespace meevax
