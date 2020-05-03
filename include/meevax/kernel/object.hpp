#ifndef INCLUDED_MEEVAX_KERNEL_OBJECT_HPP
#define INCLUDED_MEEVAX_KERNEL_OBJECT_HPP

#include <meevax/kernel/pointer.hpp>

namespace meevax::kernel
{
  /* ==== Object Facade ========================================================
  *
  *
  * ========================================================================= */
  template <typename T>
  struct alignas(category_mask + 1) objective
  {
    virtual auto type() const noexcept
      -> const std::type_info&
    {
      return typeid(T);
    }

    virtual auto copy() const
      -> std::shared_ptr<T>
    {
      if constexpr (std::is_copy_constructible<T>::value)
      {
        return std::make_shared<T>(static_cast<const T&>(*this));
      }
      else
      {
        static_assert(
          []() constexpr { return false; }(),
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

  struct pair; // forward declaration

  // TODO Rename => identifier
  using object = pointer<pair>;

  // TODO Rename => cons
  using resource = std::allocator<object>;

  template <typename T, typename... Ts>
  inline constexpr decltype(auto) make(Ts&&... operands)
  {
    return
      object::make_binding<T>(
        std::forward<decltype(operands)>(operands)...);
  }

  template <typename T,
            typename MemoryResource, // XXX (GCC-9 <=)
            typename... Ts>
  inline constexpr decltype(auto) allocate(MemoryResource&& resource, Ts&&... operands)
  {
    return
      object::allocate_binding<T>(
        std::forward<decltype(resource)>(resource),
        std::forward<decltype(operands)>(operands)...);
  }

  static const object unit {nullptr};

  #define DEFINE_MISCELLANEOUS(TYPENAME)                                       \
  struct TYPENAME##_behavior                                                   \
  {                                                                            \
    friend auto operator<<(std::ostream& os, const TYPENAME##_behavior&)       \
      -> decltype(os)                                                          \
    {                                                                          \
      return os << posix::highlight::comment << "#;" #TYPENAME                 \
                << posix::attribute::normal;                                   \
    }                                                                          \
  };                                                                           \
                                                                               \
  static const auto TYPENAME {make<TYPENAME##_behavior>()}

  DEFINE_MISCELLANEOUS(undefined);
  DEFINE_MISCELLANEOUS(unspecified);
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_OBJECT_HPP

