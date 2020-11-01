#include <meevax/kernel/closure.hpp>
#include <meevax/posix/vt102.hpp>

namespace meevax { inline namespace kernel
{
  auto operator <<(std::ostream& port, const closure& datum) -> decltype(port)
  {
    return port << magenta << "#,("
                << green << "closure" << reset
                << faint << " #;" << &datum << reset
                << magenta << ")" << reset;
  }
}} // namespace meevax::kernel
