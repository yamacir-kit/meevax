#include <iomanip>

#include <meevax/kernel/path.hpp>
#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace kernel
{
  auto operator <<(output_port & port, path const& datum) -> output_port &
  {
    return port << cyan << "#p" << std::quoted(datum.c_str()) << reset;
  }
} // namespace kernel
} // namespace meevax
