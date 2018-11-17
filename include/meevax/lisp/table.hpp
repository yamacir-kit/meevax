#ifndef INCLUDED_MEEVAX_LISP_TABLE_HPP
#define INCLUDED_MEEVAX_LISP_TABLE_HPP

#include <cassert>
#include <string>
#include <unordered_map>

#include <meevax/lisp/cell.hpp>
#include <meevax/utility/type_erasure.hpp>

namespace meevax::lisp
{
  auto intern(const std::string& s, std::unordered_map<std::string, cursor>& table)
    -> const auto&
  {
    if (const auto& iter {table.find(s)}; iter != std::end(table))
    {
      return iter->second;
    }
    else return table.emplace(
      s, std::make_shared<utility::binder<std::string, cell>>(s)
    ).first->second;
  }

  // returns unchecked reference
  auto lookup(const std::string& s, const std::unordered_map<std::string, cursor>& table)
    -> const auto&
  {
    const auto& iter {table.find(s)};
    assert(iter != std::end(table));
    return iter->second;
  }
};

#endif // INCLUDED_MEEVAX_LISP_TABLE_HPP

