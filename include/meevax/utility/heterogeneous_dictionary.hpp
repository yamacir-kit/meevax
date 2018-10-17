#ifndef INCLUDED_MEEVAX_UTILITY_HETEROGENEOUS_DICTIONARY_HPP
#define INCLUDED_MEEVAX_UTILITY_HETEROGENEOUS_DICTIONARY_HPP

#include <cassert>
#include <string>
#include <unordered_map>
#include <utility>

namespace meevax::utility
{
  template <typename ErasureBasePointer, typename TypeEraseOperation>
  class heterogeneous_dictionary
    : public std::unordered_map<std::string, ErasureBasePointer>
  {
    static constexpr std::size_t car {0};
    static constexpr std::size_t cdr {1};

    using base_type = std::unordered_map<std::string, ErasureBasePointer>;

    static TypeEraseOperation erase;

  public:
    template <typename... Ts>
    explicit heterogeneous_dictionary(Ts&&... args)
      : base_type {std::forward<Ts>(args)...}
    {}

    // returns object immutable reference and whether object generation occurred.
    const auto& intern_(const std::string& s)
    {
      const auto [iter, inserted] {emplace(s, erase(s))};
      return std::make_pair(iter->second, inserted);
    }

    const auto& intern(const std::string& s)
    {
      base_type::emplace(s, erase(s));
      return base_type::at(s);
    }

    const auto& unchecked_reference(const std::string& s) const noexcept
    {
      const auto iter {base_type::find(s)};
      assert(iter != std::end(*this));
      return std::get<cdr>(*iter);
    }
  };
} // namespace meevax::utility

#endif // INCLUDED_MEEVAX_UTILITY_HETEROGENEOUS_DICTIONARY_HPP

