#ifndef INCLUDED_MEEVAX_KERNEL_PREFACE_HPP
#define INCLUDED_MEEVAX_KERNEL_PREFACE_HPP

#include <meevax/functional/identity.hpp>

#define NIL /* nothing */

namespace meevax
{
inline namespace kernel
{
  template <typename T>
  class pointer;

  struct pair;

  using object = pointer<pair>;

  using let = object;

  using  input_port = std::istream;
  using output_port = std::ostream;

  template <typename... Ts>
  using define = typename identity<Ts...>::type;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PREFACE_HPP
