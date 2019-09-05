#ifndef INCLUDED_MEEVAX_SYSTEM_PATH_HPP
#define INCLUDED_MEEVAX_SYSTEM_PATH_HPP

#include <experimental/filesystem>

#include <meevax/system/object.hpp>
#include <meevax/system/writer.hpp>

namespace meevax::system
{
  struct path
    : public std::experimental::filesystem::path
  {
    template <typename... Ts>
    explicit constexpr path(Ts&&... xs)
      : std::experimental::filesystem::path {std::forward<Ts>(xs)...}
    {}
  };

  auto operator<<(std::ostream& os, const path& path)
    -> decltype(os)
  {
    return os << "\x1b[35m" << "#("
              << "\x1b[32m" << "path"
              << "\x1b[36m" << " \"" << path.c_str() << "\""
              << "\x1b[35m" << ")"
              << "\x1b[0m";
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_PATH_HPP

