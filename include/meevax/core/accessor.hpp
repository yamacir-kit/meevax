#ifndef INCLUDED_MEEVAX_CORE_ACCESSOR_HPP
#define INCLUDED_MEEVAX_CORE_ACCESSOR_HPP

#include <memory>
#include <utility>

namespace meevax::core
{
  template <typename T>
  struct accessor
    : public std::shared_ptr<T>,
      public std::iterator<
               std::input_iterator_tag,
               typename std::shared_ptr<T>::element_type
             >
  {
    template <typename... Ts>
    constexpr accessor(Ts&&... args)
      : std::shared_ptr<T> {std::forward<Ts>(args)...}
    {}

    #define meevax_core_indirect_accessor(name, index) \
    friend decltype(auto) name(const accessor<T>& accessor) noexcept \
    { \
      return std::get<index>(accessor.std::shared_ptr<T>::operator*()); \
    }

    meevax_core_indirect_accessor(car, 0);
    meevax_core_indirect_accessor(cdr, 1);

    decltype(auto) operator*() const noexcept
    {
      return car(*this);
    }

    decltype(auto) operator++() noexcept
    {
      return *this = cdr(*this);
    }
  };
} // namespace meevax::core

#endif // INCLUDED_MEEVAX_CORE_ACCESSOR_HPP

