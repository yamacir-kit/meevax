#include <meevax/kernel/closure.hpp>
#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace kernel
{
  auto operator <<(std::ostream & port, const closure& datum) -> std::ostream &
  {
    return port << magenta << "#,("
                << green << "closure" << reset
                << faint << " #;" << &datum << reset
                << magenta << ")" << reset;
  }
} // namespace kernel
} // namespace meevax
