#include <meevax/kernel/vector.hpp>
#include <meevax/posix/vt102.hpp>

namespace meevax
{
inline namespace kernel
{
  auto operator ==(const vector& lhs, const vector& rhs) -> bool
  {
    return std::equal(std::begin(lhs), std::end(lhs), std::begin(rhs), std::end(rhs), equal);
  }

  auto operator <<(std::ostream& port, const vector& datum) -> decltype(port)
  {
    port << magenta << "#(" << reset;

    for (auto iter { std::begin(datum) }; iter != std::end(datum); ++iter)
    {
      port << *iter << (std::next(iter) != std::end(datum) ? " " : "");
    }

    return port << magenta << ")" << reset;
  }
} // namespace kernel
} // namespace meevax
