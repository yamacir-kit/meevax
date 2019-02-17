#ifndef INCLUDED_MEEVAX_CORE_ACCESSOR_HPP
#define INCLUDED_MEEVAX_CORE_ACCESSOR_HPP

#include <iostream>
#include <memory>
#include <type_traits>
#include <typeinfo>
#include <utility>

namespace meevax::core
{
  template <typename T>
  struct universal_base
  {
    virtual auto type() const noexcept
      -> const std::type_info&
    {
      return typeid(T);
    }

    // template <typename U>
    // decltype(auto) as() const noexcept(false)
    // {
    //   return dynamic_cast<const U&>(*this);
    // }

    // Type T is able to customize print function via stream output operator.
    virtual std::ostream& write(std::ostream& os) const
    {
      return os << static_cast<const T&>(*this);
    };
  };

  template <typename UniversalBaseType = universal_base<void>>
  struct accessor
    : public std::shared_ptr<UniversalBaseType>
  {
    // We use parallel inheritance to bind any typed object because there is no
    // guarantiee for bound type is static-castable from/to universal base type.
    template <typename BoundType>
    struct binder
      : public BoundType,
        public virtual UniversalBaseType
    {
      template <typename... Ts>
      explicit constexpr binder(Ts&&... args)
        : std::conditional<
            std::is_base_of<UniversalBaseType, BoundType>::value, UniversalBaseType, BoundType
          >::type {std::forward<Ts>(args)...}
      {}

      auto type() const noexcept
        -> const std::type_info& override
      {
        return typeid(BoundType);
      }

    private:
      // Override UniversalBaseType::write(), then invoke BoundType's stream output operator.
      std::ostream& write(std::ostream& os) const override
      {
        return os << static_cast<const BoundType&>(*this);
      }
    };

  public:
    template <typename... Ts>
    constexpr accessor(Ts&&... args)
      : std::shared_ptr<UniversalBaseType> {std::forward<Ts>(args)...}
    {}

    // If you initialize `accessor<TopType>` by `accessor<TopType>::bind<BoundType>(args...)`,
    // `std::shared_ptr<TopType>` remembers it has assigned `accessor<TopType>::binder<BoundType>` originally,
    // thus both TopType and BoundType's destructor will works correctly.
    template <typename BoundType, typename... Ts>
    static constexpr auto bind(Ts&&... args)
      -> accessor<UniversalBaseType>
    {
      using bindings = binder<BoundType>;
      return std::make_shared<bindings>(std::forward<Ts>(args)...);
    }

    decltype(auto) access()       { return std::shared_ptr<UniversalBaseType>::operator*(); }
    decltype(auto) access() const { return std::shared_ptr<UniversalBaseType>::operator*(); }

    template <typename T>
    decltype(auto) is() const
    {
      return access().type() == typeid(T);
    }

    template <typename T>
    decltype(auto) as()
    {
      return dynamic_cast<T&>(access());
    }

    template <typename T>
    decltype(auto) as() const
    {
      return dynamic_cast<T&>(access());
    }

  public: // stack supports
    // using size_type = std::size_t;
    //
    // using reference = accessor<T>&;
    // using const_reference = const reference;
    //
    // decltype(auto) back() noexcept
    // {
    //   return *this ? operator*() : *this;
    // }
    //
    // decltype(auto) push_back(const accessor<T>& access)
    // {
    //   return *this = std::make_shared<T>(access, *this);
    // }
    //
    // decltype(auto) pop_back()
    // {
    //   return operator++();
    // }
    //
    // template <typename... Ts>
    // decltype(auto) emplace_back(Ts&&... args)
    // {
    //   return *this = std::make_shared<T>(std::forward<Ts>(args)..., *this);
    // }
  };

  // Invoke UniversalBaseType::write()
  template <typename T>
  std::ostream& operator<<(std::ostream& os, const accessor<T>& rhs)
  {
    // TODO Provide custamizable printer for nullptr.
    return !rhs ? (os << "()") : rhs.access().write(os);
  }
} // namespace meevax::core

#endif // INCLUDED_MEEVAX_CORE_ACCESSOR_HPP

