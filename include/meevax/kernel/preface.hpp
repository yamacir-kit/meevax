#ifndef INCLUDED_MEEVAX_KERNEL_PREFACE_HPP
#define INCLUDED_MEEVAX_KERNEL_PREFACE_HPP

namespace meevax
{
inline namespace kernel
{
  template <typename T>
  class pointer;

  struct pair;

  using object = pointer<pair>;

  using let = object;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PREFACE_HPP
