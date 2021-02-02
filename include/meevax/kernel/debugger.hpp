#ifndef INCLUDED_MEEVAX_KERNEL_DEBUGGER_HPP
#define INCLUDED_MEEVAX_KERNEL_DEBUGGER_HPP


namespace meevax
{
inline namespace kernel
{
  template <typename SK>
  class debugger
  {
    friend SK;

    debugger()
    {}

  public:
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_DEBUGGER_HPP
