#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/list.hpp> // for eq?
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

  auto if_(let const& x) -> bool
  {
    return not eq(x, f) or not eqv(x, f);
  }
} // namespace kernel
} // namespace meevax
