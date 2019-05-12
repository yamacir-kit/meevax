#ifndef INCLUDED_MEEVAX_SYSTEM_ACCESSOR_HPP
#define INCLUDED_MEEVAX_SYSTEM_ACCESSOR_HPP

#include <iostream> // std::ostream
#include <memory> // std::shared_ptr
#include <type_traits> // std::conditional
#include <typeinfo> // typeid
#include <utility> // std::forward

#include <meevax/system/exception.hpp>

namespace meevax::system
{
  template <typename T>
  struct facade
  {
    virtual auto type() const noexcept
      -> const std::type_info&
    {
      return typeid(T);
    }

    virtual std::shared_ptr<T> copy() const
    {
      return std::make_shared<T>(static_cast<const T&>(*this));
    }

    // // (1)
    // virtual T& operator=(const T& rhs)
    // {
    //   std::cerr << "[debug] (1) " << __LINE__ << std::endl;
    //   return static_cast<T&>(*this) = rhs;
    // }
    //
    // virtual void placement_copy(T* place) const
    // {
    //   new (place) T {static_cast<const T&>(*this)};
    // }

    virtual std::ostream& write(std::ostream& os) const
    {
      return os << static_cast<const T&>(*this);
    };
  };

  template <typename TopType = facade<void>>
  class accessor
    : public std::shared_ptr<TopType>
  {
    template <typename BoundType>
    struct binder
      : public BoundType
      , public virtual TopType
    {
      template <typename... Ts>
      explicit constexpr binder(Ts&&... args)
        : std::conditional<
            // XXX 注意
            // トップ型を仮想継承した型をバインドする場合は、コンストラクタ引数をすべて基底クラスに流し込む
            // かなりクセのある挙動だが、初期化タイミング都合こうするしか無さそう
            std::is_base_of<TopType, BoundType>::value, TopType, BoundType
            // TODO std::is_virtual_base_of があるなら置き換えること
          >::type {std::forward<Ts>(args)...}
      {}

      auto type() const noexcept
        -> const std::type_info& override
      {
        return typeid(BoundType);
      }

    private:
      // XXX BoundType required CopyConstructible?
      std::shared_ptr<TopType> copy() const override
      {
        using binding = binder<BoundType>;
        return std::make_shared<binding>(*this);
      }

      // void placement_copy(TopType* place) const override
      // {
      //   new (place) binder<BoundType> {static_cast<const BoundType&>(*this)};
      // }
      //
      // // (1')
      // TopType& operator=(const TopType& rhs) override
      // {
      //   std::cerr << "[debug] (1')\n";
      //   std::cerr << "        lhs: " << utility::demangle(type()) << "\n"
      //             << "        rhs: " << utility::demangle(rhs.type()) << std::endl;
      //
      //   if (type() != rhs.type())
      //   {
      //     // rhs.placement_copy(this); // XXX WORKS BUT GET SEGV
      //     return *this;
      //   }
      //   else
      //   {
      //     static_cast<BoundType&>(*this) = dynamic_cast<const BoundType&>(rhs);
      //     return *this;
      //   }
      // }

      // Override TopType::write(), then invoke BoundType's stream output operator.
      std::ostream& write(std::ostream& os) const override
      {
        return os << static_cast<const BoundType&>(*this);
      }
    };

  public:
    template <typename... Ts>
    constexpr accessor(Ts&&... args)
      : std::shared_ptr<TopType> {std::forward<Ts>(args)...}
    {}

    // If you initialize accessor<TopType> by accessor<TopType>::bind<BoundType>(args...),
    // std::shared_ptr<TopType> remembers it has assigned accessor<TopType>::binder<BoundType> originally,
    // thus both TopType and BoundType's destructor will works correctly.
    template <typename BoundType, typename... Ts>
    static constexpr auto bind(Ts&&... args)
      -> accessor<TopType>
    {
      using binding = binder<BoundType>;
      return std::make_shared<binding>(std::forward<Ts>(args)...);
    }

    decltype(auto) access()
    {
      if (*this)
      {
        return std::shared_ptr<TopType>::operator*();
      }
      else
      {
        // This exception occurrence is guarded by selecter
        throw error {"accessing to unit"};
      }
    }

    decltype(auto) access() const
    {
      if (*this)
      {
        return std::shared_ptr<TopType>::operator*();
      }
      else
      {
        // This exception occurrence is guarded by selecter
        throw error {"accessing to unit"};
      }
    }

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
      return dynamic_cast<const T&>(access());
    }
  };

  // Invoke TopType::write()
  template <typename T>
  std::ostream& operator<<(std::ostream&, const accessor<T>&);
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_ACCESSOR_HPP

