#include <meevax/string/indent.hpp>

namespace meevax
{
inline namespace string
{
  auto operator <<(std::ostream & os, indent const& datum) -> std::ostream &
  {
    return os << static_cast<std::string>(datum);
  }

  auto operator <<(indent & datum, std::size_t width) -> indent &
  {
    indent::depth -= std::min(indent::depth, width);
    return datum;
  }

  auto operator <<(indent && datum, std::size_t width) -> indent &
  {
    indent::depth -= std::min(indent::depth, width);
    return datum;
  }

  auto operator >>(indent & datum, std::size_t width) -> indent &
  {
    indent::depth += width;
    return datum;
  }

  auto operator >>(indent && datum, std::size_t width) -> indent &
  {
    indent::depth += width;
    return datum;
  }
} // namespace string
} // namespace meevax
