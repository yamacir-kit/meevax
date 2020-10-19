#include <meevax/kernel/ghost.hpp>

namespace meevax { inline namespace kernel
{
  auto operator <<(std::ostream& os, const unspecified_t&) -> decltype(os)
  {
    return os << faint << "#;unspecified" << reset;
  }

  let const unspecified { make<unspecified_t>() };

  auto operator <<(std::ostream& os, const undefined_t&) -> decltype(os)
  {
    return os << faint << "#;undefined" << reset;
  }

  let const undefined { make<undefined_t>() };
}} // namespace meevax::kernel
