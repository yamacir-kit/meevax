#ifndef INCLUDED_MEEVAX_SYSTEM_PATH_HPP
#define INCLUDED_MEEVAX_SYSTEM_PATH_HPP

#include <experimental/filesystem>

#include <meevax/system/object.hpp>

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
    return os << highlight::syntax << "#("
              << highlight::constructor << "path"
              << highlight::simple_datum << " \"" << path.c_str() << "\""
              << highlight::syntax << ")"
              << attribute::normal;
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_PATH_HPP

