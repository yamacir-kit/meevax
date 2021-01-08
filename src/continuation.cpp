#include <meevax/kernel/continuation.hpp>
#include <meevax/posix/vt102.hpp>

namespace meevax
{
inline namespace kernel
{
  auto operator <<(std::ostream& port, const continuation& datum) -> decltype(port)
  {
    return port << magenta << "#,("
                << green << "continuation" << reset
                << faint << " ;#" << &datum << reset
                << magenta << ")" << reset;
  }
} // namespace kernel
} // namespace meevax
