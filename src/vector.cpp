#include <boost/range/adaptor/reversed.hpp>
#include <meevax/algorithm/for_each.hpp>
#include <meevax/kernel/stack.hpp>
#include <meevax/kernel/vector.hpp>
#include <meevax/posix/vt102.hpp>

namespace meevax
{
inline namespace kernel
{
  let vector::to_list() const
  {
    let x = unit;

    for (auto const& each : boost::adaptors::reverse(*this))
    {
      push(x, each);
    }

    return x;
  }

  auto operator ==(vector const& lhs, vector const& rhs) -> bool
  {
    return std::equal(std::begin(lhs), std::end(lhs),
                      std::begin(rhs), std::end(rhs), equal);
  }

  auto operator <<(std::ostream & port, vector const& datum) -> decltype(port)
  {
    return port << magenta << "#(" << reset << for_each(datum) << magenta << ")" << reset;
  }
} // namespace kernel
} // namespace meevax
