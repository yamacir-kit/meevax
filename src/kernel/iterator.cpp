#include <meevax/kernel/iterator.hpp>

namespace std
{
  auto cbegin(meevax::object const& x) -> meevax::homoiconic_iterator<meevax::object const> { return x; }
  auto  begin(meevax::object const& x) -> meevax::homoiconic_iterator<meevax::object const> { return x; }
  auto   cend(meevax::object const&  ) -> meevax::homoiconic_iterator<meevax::object const> { return meevax::unit; }
  auto    end(meevax::object const&  ) -> meevax::homoiconic_iterator<meevax::object const> { return meevax::unit; }
} // namespace std
