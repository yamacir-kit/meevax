#include <meevax/kernel/error.hpp>

namespace meevax
{
inline namespace kernel
{
  auto operator <<(std::ostream& port, const error& datum) -> std::ostream&
  {
    return port << magenta << "#("
                << green << "error "
                << cyan << std::quoted(datum.what())
                << magenta << ")"
                << reset;
  }
} // namespace kernel
} // namespace meevax
