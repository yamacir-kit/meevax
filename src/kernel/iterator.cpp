#include <meevax/kernel/iterator.hpp>

namespace std
{
  auto cbegin(meevax::let const& x) -> meevax::homoiconic_iterator<meevax::let const> { return x; }
  auto  begin(meevax::let const& x) -> meevax::homoiconic_iterator<meevax::let const> { return x; }
  auto   cend(meevax::let const&  ) -> meevax::homoiconic_iterator<meevax::let const> { return meevax::unit; }
  auto    end(meevax::let const&  ) -> meevax::homoiconic_iterator<meevax::let const> { return meevax::unit; }
} // namespace std
