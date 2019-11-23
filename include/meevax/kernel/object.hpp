#ifndef INCLUDED_MEEVAX_KERNEL_OBJECT_HPP
#define INCLUDED_MEEVAX_KERNEL_OBJECT_HPP

#include <meevax/kernel/pointer.hpp>

namespace meevax::kernel
{
  /* ==== Object Facade =======================================================
  *
  *
  *========================================================================== */
  template <typename T>
  struct alignas(category_mask + 1) objective
  {
    virtual auto type() const noexcept
      -> const std::type_info&
    {
      return typeid(T);
    }

    virtual std::shared_ptr<T> copy() const
    {
      if constexpr (std::is_copy_constructible<T>::value)
      {
        return std::make_shared<T>(static_cast<const T&>(*this));
      }
      else
      {
        static_assert(
          []() { return false; }(),
          "The base type of meevax::kernel::pointer requires concept CopyConstructible.");
      }
    }

    virtual bool equivalent_to(const std::shared_ptr<T>& other) const
    {
      if constexpr (concepts::is_equality_comparable<T>::value)
      {
        const auto p {std::dynamic_pointer_cast<const T>(other)};
        assert(p);
        return static_cast<const T&>(*this) == *p;
      }
      else
      {
        // TODO: warning
        return false;
      }
    }

    virtual auto dispatch(std::ostream& os) const
      -> decltype(os)
    {
      return os << static_cast<const T&>(*this);
    };
  };

  // forward declaration
  struct pair;

  using object = pointer<pair>;

  template <typename T, typename... Ts>
  constexpr decltype(auto) make(Ts&&... operands)
  {
    return
      object::bind<T>(
        std::forward<decltype(operands)>(operands)...);
  }

  extern "C" const object unit, unbound, undefined, unspecified;
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_OBJECT_HPP

