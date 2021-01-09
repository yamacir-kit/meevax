#include <boost/range/adaptors.hpp>
#include <meevax/algorithm/for_each.hpp>
#include <meevax/kernel/stack.hpp>
#include <meevax/kernel/vector.hpp>
#include <meevax/posix/vt102.hpp>

namespace meevax
{
inline namespace kernel
{
  let vector::to_list(vector::size_type from, vector::size_type to) const
  {
    using boost::adaptors::reverse;
    using boost::adaptors::slice;

    let x = unit;

    for (auto const& each : reverse(slice(*this, from, to)))
    {
      push(x, each);
    }

    return x;
  }

  // let vector::to_string()
  // {
  //   let s = unit;
  //
  //   for (auto const& each : boost::adapters::reverse(*this))
  //   {
  //     s = make<string>()
  //   }
  // }

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
