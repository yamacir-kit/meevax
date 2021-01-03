#include <meevax/posix/vt102.hpp>

namespace meevax
{
inline namespace posix
{
  auto operator <<(std::ostream& port, const cursor_move& datum) -> decltype(port)
  {
    return escape_sequence(port, datum.value, datum.code);
  }
} // namespace posix
} // namespace meevax
