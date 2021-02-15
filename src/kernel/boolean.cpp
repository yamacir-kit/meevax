#include <meevax/kernel/boolean.hpp>
#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace kernel
{
  auto operator <<(std::ostream & port, const boolean& datum) -> std::ostream &
  {
    return port << cyan << "#" << std::boolalpha << datum.value << reset;
  }

  let const t = make<boolean>(true);
  let const f = make<boolean>(false);
} // namespace kernel
} // namespace meevax
