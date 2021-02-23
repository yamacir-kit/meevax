#include <boost/range/adaptors.hpp>
#include <meevax/algorithm/for_each.hpp>
#include <meevax/kernel/error.hpp>
#include <meevax/kernel/stack.hpp>
#include <meevax/kernel/string.hpp>
#include <meevax/kernel/vector.hpp>
#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace kernel
{
  let vector::fill(let const& value, vector::size_type from, vector::size_type to)
  {
    using boost::adaptors::sliced;

    for (auto&& each : *this | sliced(from, to))
    {
      each = value;
    }

    return value;
  }

  let vector::to_list(vector::size_type from, vector::size_type to) const
  {
    using boost::adaptors::reversed;
    using boost::adaptors::sliced;

    let x = unit;

    for (let const& each : *this | sliced(from, to) | reversed)
    {
      push(x, each);
    }

    return x;
  }

  let vector::to_string(vector::size_type from, vector::size_type to) const
  {
    using boost::adaptors::sliced;

    string s;

    for (let const& each : *this | sliced(from, to))
    {
      if (each.is<character>())
      {
        s.push_back(each.as<character>());
      }
      else
      {
        throw make<error_>(
          make<string>("It is an error if any element of vector between start and end is not a character."),
          unit);
      }
    }

    return make(s);
  }

  auto operator ==(vector const& lhs, vector const& rhs) -> bool
  {
    return std::equal(std::begin(lhs), std::end(lhs),
                      std::begin(rhs), std::end(rhs), equal);
  }

  auto operator <<(std::ostream & port, vector const& datum) -> std::ostream &
  {
    return port << magenta << "#(" << reset << for_each(datum) << magenta << ")" << reset;
  }
} // namespace kernel
} // namespace meevax
