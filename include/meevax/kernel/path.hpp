#ifndef INCLUDED_MEEVAX_KERNEL_PATH_HPP
#define INCLUDED_MEEVAX_KERNEL_PATH_HPP

#include <experimental/filesystem>

#include <meevax/kernel/object.hpp>

namespace meevax::kernel
{
  struct path
    : public std::experimental::filesystem::path
  {
    using identity = path;

    using std::experimental::filesystem::path::path;

    friend auto operator<<(std::ostream& os, const identity& i)
      -> decltype(os)
    {
      return os << highlight::syntax << "#("
                << highlight::type << "path"
                << highlight::datum << " \"" << i.c_str() << "\""
                << highlight::syntax << ")"
                << attribute::normal;
    }
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_PATH_HPP

