#ifndef INCLUDED_MEEVAX_SYSTEM_ACCESSOR_HPP
#define INCLUDED_MEEVAX_SYSTEM_ACCESSOR_HPP

#include <cassert>
#include <iostream> // std::ostream
#include <memory> // std::shared_ptr
#include <type_traits> // std::conditional
#include <typeinfo> // typeid
#include <utility> // std::forward

#include <meevax/concepts/is_equality_comparable.hpp>
#include <meevax/concepts/is_stream_insertable.hpp>

// #include <boost/type_traits/is_virtual_base_of.hpp>
//
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

#include <meevax/system/exception.hpp>
#include <meevax/utility/demangle.hpp>

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
      if constexpr (std::is_copy_constructible<T>::value)
      {
        return std::make_shared<T>(static_cast<const T&>(*this));
      }
      else
      {
        using meevax::utility::demangle;
        throw error {"from ", demangle(typeid(*this)), "::copy(), type ", demangle(typeid(T)), " is not copy-constructible."};
      }
    }

    // eqv?
    virtual bool equals(const std::shared_ptr<T>& rhs) const
    {
      if constexpr (concepts::is_equality_comparable<T>::value)
      {
        const auto rhs_ {std::dynamic_pointer_cast<const T>(rhs)};
        assert(rhs_);
        return static_cast<const T&>(*this) == *rhs_;
      }
      else
      {
        return false;
      }
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
        : std::conditional< // transfers all arguments if Bound Type inherits Top Type virtually.
            std::is_base_of<TopType, BoundType>::value, TopType, BoundType
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
          using meevax::utility::demangle;
          throw error {"from ", demangle(typeid(*this)), "::copy(), type ", demangle(typeid(BoundType)), " is not copy-constructible."};
        }
      }

      bool equals(const std::shared_ptr<TopType>& rhs) const override
      {
        if constexpr (concepts::is_equality_comparable<BoundType>::value)
        {
          return static_cast<const BoundType&>(*this) == *std::dynamic_pointer_cast<const BoundType>(rhs);
        }
        else
        {
          return false;
        }
      }

      // Override TopType::write(), then invoke BoundType's stream output operator.
      std::ostream& write(std::ostream& os) const override
      {
        if constexpr (concepts::is_stream_insertable<BoundType>::value)
        {
          return os << static_cast<const BoundType&>(*this);
        }
        else
        {
          return os << "\x1b[36m#<" << utility::demangle(typeid(BoundType)) << " " << static_cast<const BoundType*>(this) << ">\x1b[0m";
        }
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

    decltype(auto) dereference() const
    {
      if (*this)
      {
        return std::shared_ptr<TopType>::operator*();
      }
      else
      {
        throw error {"segmentation fault guarded"};
      }
    }

    #define SHORT_ACCESS(NAME) \
    decltype(auto) NAME() const \
    { \
      return dereference().NAME(); \
    }

    SHORT_ACCESS(type);
    SHORT_ACCESS(copy);

    template <typename T>
    decltype(auto) is() const
    {
      return type() == typeid(T);
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
      return dynamic_cast<const T&>(dereference()); // TODO dynamic_pointer_cast
    }

    template <typename T>
    decltype(auto) as()
    {
      return dynamic_cast<T&>(dereference());
    }

    bool equals(const accessor& rhs) const
    {
      if (type() != rhs.type())
      {
        return false;
      }
      else
      {
        return dereference().equals(rhs);
      }
    }
  };

  // Invoke TopType::write()
  template <typename T>
  std::ostream& operator<<(std::ostream& os, const accessor<T>& object)
  {
    // write(os) will be dispatched to each type's stream output operator.
    return !object ? (os << "\x1b[35m()\x1b[0m") : object.dereference().write(os);
  }

} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_ACCESSOR_HPP

