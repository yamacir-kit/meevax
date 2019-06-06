#include <iostream>
#include <memory>
#include <string>
#include <type_traits>
#include <typeinfo>

struct object
{
  virtual const std::type_info& type() const noexcept = 0;

  virtual ~object() = default;
};

template <typename T>
struct flag;

template <>
struct flag<std::int32_t> // 32ビットコンピュータの場合はバグる
  : public std::integral_constant<unsigned int, 0x01>
{};

constexpr unsigned int mask {0x07};

using any = void;

template <typename T>
constexpr auto masked_value_of(T* pointer)
{
  return reinterpret_cast<std::uintptr_t>(pointer) & mask;
}

template <typename... Ts>
constexpr bool is_tagged_pointer(Ts&&... args)
{
  return masked_value_of(std::forward<Ts>(args)...);
}

auto delete_if_is_not_tagged_pointer = [](auto* pointer)
{
  if (not is_tagged_pointer(pointer))
  {
    std::cout << "[deleter] is not tagged pointer => delete" << std::endl;
    delete pointer;
  }
  else
  {
    std::cout << "[deleter] is tagged pointer => nop" << std::endl;
  }
};

class pointer
  : public std::shared_ptr<object>
{
  template <typename T>
  struct alignas(8) binder
    : public object
    , public T
  {
    template <typename... Ts>
    constexpr binder(Ts&&... args)
      : T {std::forward<Ts>(args)...}
    {}

    const std::type_info& type() const noexcept override
    {
      return typeid(T);
    }
  };

public:
  template <typename... Ts>
  constexpr pointer(Ts&&... args)
    : std::shared_ptr<object> {std::forward<Ts>(args)...}
  {}

  // どのみちスカラ型は継承できないので未定義の場合はコンパイルエラーとする
  template <typename T, typename... Ts,
            typename = typename std::enable_if<not std::is_scalar<T>::value>::type>
  static pointer bind(Ts&&... args)
  {
    return std::shared_ptr<object>(new binder<T> {std::forward<Ts>(args)...}, delete_if_is_not_tagged_pointer);
  }

  // 関数テンプレートの部分特殊化が出来ないためSFINAEで代用
  template <typename T,
            typename = typename std::enable_if<
                         std::is_same<
                           typename std::decay<T>::type, std::int32_t
                         >::value
                       >::type>
  static pointer bind(T&& value)
  {
    return std::shared_ptr<object>(
             reinterpret_cast<object*>(
               (static_cast<std::uintptr_t>(value) << 3) | flag<T>::value
             ),
             delete_if_is_not_tagged_pointer
           );
  }

  template <typename T,
            typename = typename std::enable_if<not std::is_scalar<T>::value>::type>
  decltype(auto) as() const
  {
    return dynamic_cast<const T&>(std::shared_ptr<object>::operator*());
  }

  template <typename T,
            typename = typename std::enable_if<
                         std::is_same<
                           typename std::decay<T>::type, std::int32_t
                         >::value
                       >::type>
  auto as(T&& = 0) const
    -> typename std::decay<T>::type
  {
    auto* pointer {std::shared_ptr<object>::get()};

    if (masked_value_of(pointer) == flag<typename std::decay<T>::type>::value)
    {
      return reinterpret_cast<std::uintptr_t>(pointer) >> 3;
    }
    else
    {
      throw std::bad_cast {};
    }
  }

  decltype(auto) type() const noexcept
  {
    auto* pointer {std::shared_ptr<object>::get()};

    if (not is_tagged_pointer(pointer))
    {
      return std::shared_ptr<object>::operator*().type();
    }
    else if (masked_value_of(pointer) == flag<std::int32_t>::value)
    {
      return typeid(std::int32_t);
    }
    else
    {
      throw std::runtime_error {"unimplemented scalar type"};
    }
  }
};

template <typename T, typename... Ts>
decltype(auto) make(Ts&&... args)
{
  return pointer::bind<T>(std::forward<Ts>(args)...);
}

int main()
{
  pointer s {make<std::string>("hello, world!")};
  std::cout << s.type().name() << std::endl;
  std::cout << s.as<std::string>() << std::endl;
  s.reset();

  pointer i32 {make<std::int32_t>(42)};
  std::cout << i32.type().name() << std::endl;
  std::cout << i32.as<std::int32_t>() << std::endl;
  s.reset();

  return 0;
}

