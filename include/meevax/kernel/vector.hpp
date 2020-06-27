#ifndef INCLUDED_MEEVAX_KERNEL_VECTOR_HPP
#define INCLUDED_MEEVAX_KERNEL_VECTOR_HPP

#include <meevax/kernel/object.hpp>

namespace meevax::kernel
{
  struct vector
    : public std::vector<object>
  {
    using std::vector<object>::vector;

    friend auto operator<<(std::ostream& os, const vector& v) -> decltype(os)
    {
      os << console::magenta << "#(" << console::reset;

      for (auto iter { std::begin(v) }; iter != std::end(v); ++iter)
      {
        os << *iter << (std::next(iter) != std::end(v) ? " " : "");
      }

      return os << console::magenta << ")" << console::reset;
    }
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_VECTOR_HPP

