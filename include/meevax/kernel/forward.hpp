#ifndef INCLUDED_MEEVAX_KERNEL_PREFACE_HPP
#define INCLUDED_MEEVAX_KERNEL_PREFACE_HPP

#include <meevax/functional/operator.hpp>
#include <meevax/posix/vt10x.hpp>
#include <meevax/string/append.hpp>
#include <meevax/utility/hexdump.hpp>
#include <meevax/utility/requires.hpp>

#define NIL /* nothing */

#define LINE() std::cout << "; " __FILE__ ":" << __LINE__ << std::endl

#define PRINT(...) std::cout << "; " #__VA_ARGS__ " = " << (__VA_ARGS__) << std::endl

namespace meevax
{
inline namespace kernel
{
  template <template <typename...> typename Pointer, typename T>
  class heterogeneous;

  struct pair;

  using cell = heterogeneous<simple_pointer, pair>;

  using let = heterogeneous<root, pair>;

  using null = std::nullptr_t;

  using  input_port = std::istream;
  using output_port = std::ostream;

  template <typename... Ts>
  using define = typename identity<Ts...>::type;

  template <typename... Ts>
  auto make_error(Ts&&... xs)
  {
    return std::runtime_error(string_append(std::forward<decltype(xs)>(xs)...));
  }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PREFACE_HPP
