#ifndef INCLUDED_MEEVAX_SYSTEM_ACCESSOR_HPP
#define INCLUDED_MEEVAX_SYSTEM_ACCESSOR_HPP

#include <iostream> // std::ostream
#include <memory> // std::shared_ptr
#include <type_traits> // std::conditional
#include <typeinfo> // typeid
#include <utility> // std::forward

// #include <boost/type_traits/is_virtual_base_of.hpp>

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
            // boost::is_virtual_base_of<TopType, BoundType>::value, TopType, BoundType
            //
            // struct hoge
            //   : public virtual TopType
            // {
            //   hoge()            = default;
            //   hoge(const hoge&) = default;
            //
            //   hoge(hoge&& moved)
            //   {
            //     static_cast<TopType&>(*this) = static_cast<TopType&&>(moved);
            //   }
            //
            //   hoge& operator=(const hoge&) = default;
            //
            //   hoge& operator=(hoge&& moved)
            //   {
            //     static_cast<TopType&>(*this) = static_cast<TopType&&>(moved);
            //     return *this;
            //   }
            // };
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

        if constexpr (std::is_copy_constructible<binding>::value)
        {
          return std::make_shared<binding>(*this);
        }
        else
        {
          throw error {"std::is_copy_constructible<binding>::value == false"};
        }
      }

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

    // TODO
    // ポインタを返すキャストインタフェースを用意して、
    // ダイナミックキャスト後のポインタの無効値部分に型情報を埋め込む事で、
    // 将来的なコンパイラ最適化に使えるかも
    template <typename T>
    decltype(auto) as() const
    {
      // const void* before {&access()};
      // const void* casted {&dynamic_cast<const T&>(access())};
      // std::cerr << "[dynamic_cast] " << before << " => " << casted << " (" << (reinterpret_cast<std::ptrdiff_t>(before) - reinterpret_cast<std::ptrdiff_t>(casted)) << ")" << std::endl;
      return dynamic_cast<const T&>(access());
    }

    template <typename T>
    decltype(auto) as()
    {
      return dynamic_cast<T&>(access());
    }
  };

  // Invoke TopType::write()
  template <typename T>
  std::ostream& operator<<(std::ostream&, const accessor<T>&);
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_ACCESSOR_HPP

