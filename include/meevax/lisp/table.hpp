#ifndef INCLUDED_MEEVAX_LISP_TABLE_HPP
#define INCLUDED_MEEVAX_LISP_TABLE_HPP

#include <cassert>
#include <string>
#include <unordered_map>
#include <utility>

#include <meevax/lisp/cell.hpp>

namespace meevax::lisp
{
  template <typename T>
  class table
    : public std::unordered_map<std::string, const cursor>
  {
  public:
    template <typename... Ts>
    explicit table(Ts&&... args)
      : std::unordered_map<std::string, const cursor> {std::forward<Ts>(args)...}
    {}

    const auto& intern(const std::string s)
    {
      emplace(s, make_as<T>(s));
      return (*this)[s];
    }
  };

  // The single source of nil.
  // static table<std::string> symbols {std::make_pair("nil", cursor {nullptr})};

  template <typename Erasure, typename Eraser>
  class heterogeneous_dictionary
    : public std::unordered_map<std::string, Erasure>
  {
    static constexpr std::size_t car {0};
    static constexpr std::size_t cdr {1};

    using base_type = std::unordered_map<std::string, Erasure>;

    static Eraser erase;

  public:
    template <typename... Ts>
    explicit heterogeneous_dictionary(Ts&&... args)
      : std::unordered_map<std::string, Erasure> {std::forward<Ts>(args)...}
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
      cassert(iter != std::end(*this));
      return std::get<cdr>(iter);
    }
  };
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_TABLE_HPP

