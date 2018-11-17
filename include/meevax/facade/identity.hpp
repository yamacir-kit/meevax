#ifndef INCLUDED_MEEVAX_FACADE_IDENTITY_HPP
#define INCLUDED_MEEVAX_FACADE_IDENTITY_HPP

#include <typeinfo>

namespace meevax::facade
{
  template <typename T>
  struct identity
  {
    virtual ~identity() = default;

    virtual auto type() const noexcept
      -> const std::type_info&
    {
      return typeid(T);
    }

    template <typename U>
    decltype(auto) as(bool check = true) const
    {
      return check ? dynamic_cast<const U&>(*this) : *dynamic_cast<const U*>(this);
    }
  };
} // namespace meevax::facade

#endif // INCLUDED_MEEVAX_FACADE_IDENTITY_HPP

