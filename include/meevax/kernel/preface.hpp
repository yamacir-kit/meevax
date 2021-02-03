#ifndef INCLUDED_MEEVAX_KERNEL_PREFACE_HPP
#define INCLUDED_MEEVAX_KERNEL_PREFACE_HPP

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

  using bytestring = std::string;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PREFACE_HPP
