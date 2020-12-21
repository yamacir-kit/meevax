#include <meevax/kernel/boolean.hpp>
#include <meevax/posix/vt102.hpp>

namespace meevax
{
inline namespace kernel
{
  auto operator <<(std::ostream& port, const boolean& datum) -> decltype(port)
  {
    return port << cyan << "#" << std::boolalpha << datum.value << reset;
  }

  let const t = make<boolean>(true);
  let const f = make<boolean>(false);
} // namespace kernel
} // namespace meevax
