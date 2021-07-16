#ifndef INCLUDED_MEEVAX_KERNEL_VECTOR_HPP
#define INCLUDED_MEEVAX_KERNEL_VECTOR_HPP

#include <meevax/kernel/list.hpp>

namespace meevax
{
inline namespace kernel
{
  enum class for_each_in_tag {} constexpr for_each_in {};

  struct vector
    : public std::vector<let>
  {
    using std::vector<let>::vector;

    template <typename InputIterator>
    explicit vector(for_each_in_tag, InputIterator from, InputIterator to)
    {
      std::copy(from, to, std::back_inserter(*this));
    }

    explicit vector(for_each_in_tag, let const& xs)
      : vector { for_each_in, std::cbegin(xs), std::cend(xs) }
    {}

    let fill(let const& value, size_type, size_type);

    decltype(auto) fill(let const& value, size_type from = 0)
    {
      return fill(value, from, size());
    }

    decltype(auto) fill(let const& value, let const& from)
    {
      return fill(value, from.as<exact_integer>().to<size_type>());
    }

    decltype(auto) fill(let const& value, let const& from, let const& to)
    {
      return fill(value, from.as<exact_integer>().to<size_type>(),
                         to  .as<exact_integer>().to<size_type>());
    }

    let to_list(size_type, size_type) const;

    let to_string(size_type, size_type) const;

    #define DEFINE_RANGE_OVERLOADS_FOR(NAME)                                   \
    decltype(auto) NAME(size_type from = 0)                                    \
    {                                                                          \
      return NAME(from, size());                                               \
    }                                                                          \
                                                                               \
    decltype(auto) NAME(let const& from)                                       \
    {                                                                          \
      return NAME(from.as<exact_integer>().to<size_type>());                   \
    }                                                                          \
                                                                               \
    decltype(auto) NAME(let const& from, let const& to)                        \
    {                                                                          \
      return NAME(from.as<exact_integer>().to<size_type>(),                    \
                  to  .as<exact_integer>().to<size_type>());                   \
    }                                                                          \
                                                                               \
    static_assert(true)

    DEFINE_RANGE_OVERLOADS_FOR(to_list);
    DEFINE_RANGE_OVERLOADS_FOR(to_string);

    #undef DEFINE_RANGE_OVERLOADS_FOR
  };

  auto operator ==(vector const&, vector const&) -> bool;

  auto operator <<(std::ostream & port, vector const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_VECTOR_HPP
