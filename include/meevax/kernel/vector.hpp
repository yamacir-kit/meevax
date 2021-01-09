#ifndef INCLUDED_MEEVAX_KERNEL_VECTOR_HPP
#define INCLUDED_MEEVAX_KERNEL_VECTOR_HPP

#include <meevax/kernel/list.hpp>

namespace meevax
{
inline namespace kernel
{
  enum class for_each_in_tag {} constexpr for_each_in {};

  struct vector
    : public std::vector<object>
  {
    using std::vector<object>::vector;

    template <typename InputIterator>
    explicit vector(for_each_in_tag, InputIterator&& begin, InputIterator&& end)
      : std::vector<object> {}
    {
      std::copy(begin, end, std::back_inserter(*this));
    }

    explicit vector(for_each_in_tag, value_type const& xs)
      : vector { for_each_in, std::cbegin(xs), std::cend(xs) }
    {}

    let to_list(size_type, size_type) const;

    decltype(auto) to_list(size_type from = 0) const
    {
      return to_list(from, size());
    }

    decltype(auto) to_list(let const& from) const
    {
      return to_list(from.as<exact_integer>().to<size_type>());
    }

    decltype(auto) to_list(let const& from, let const& to) const
    {
      return to_list(from.as<exact_integer>().to<size_type>(),
                     to  .as<exact_integer>().to<size_type>());
    }

    let to_string(size_type, size_type) const;

    decltype(auto) to_string(size_type from = 0) const
    {
      return to_string(from, size());
    }

    decltype(auto) to_string(let const& from) const
    {
      return to_string(from.as<exact_integer>().to<size_type>());
    }

    decltype(auto) to_string(let const& from, let const& to) const
    {
      return to_string(from.as<exact_integer>().to<size_type>(),
                       to  .as<exact_integer>().to<size_type>());
    }
  };

  auto operator ==(vector const&, vector const&) -> bool;

  auto operator <<(std::ostream & port, vector const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_VECTOR_HPP
