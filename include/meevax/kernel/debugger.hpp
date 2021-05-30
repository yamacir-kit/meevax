#ifndef INCLUDED_MEEVAX_KERNEL_DEBUGGER_HPP
#define INCLUDED_MEEVAX_KERNEL_DEBUGGER_HPP

namespace meevax
{
inline namespace kernel
{
  template <typename Module>
  class debugger
  {
    friend Module;

    IMPORT(Module, standard_debug_port, const);

  public:

    struct tracker
    {
      explicit tracker(let const&)
      {
        std::cout << header("debugger") << "construct tracker " << this << std::endl;
      }

      ~tracker()
      {
        std::cout << header("debugger") << "destruct tracker " << this << std::endl;
      }

      friend auto operator <<(output_port & port, tracker const& datum) -> output_port &
      {
        return port << magenta << "#("
                    << green << "tracker" << reset
                    << faint << " #;" << &datum << reset
                    << magenta << ")" << reset;
      }
    };
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_DEBUGGER_HPP
