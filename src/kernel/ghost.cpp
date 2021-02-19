#include <meevax/kernel/ghost.hpp>

namespace meevax
{
inline namespace kernel
{
  auto operator <<(std::ostream& port, const unspecified_t&) -> decltype(port)
  {
    return port << faint << "#;unspecified" << reset;
  }

  let const unspecified = make<unspecified_t>();

  auto operator <<(std::ostream& port, const undefined_t&) -> decltype(port)
  {
    return port << faint << "#;undefined" << reset;
  }

  let const undefined = make<undefined_t>();
} // namespace kernel
} // namespace meevax
