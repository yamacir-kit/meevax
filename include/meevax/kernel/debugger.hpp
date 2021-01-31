#ifndef INCLUDED_MEEVAX_KERNEL_DEBUGGER_HPP
#define INCLUDED_MEEVAX_KERNEL_DEBUGGER_HPP

#include <meevax/string/indent.hpp>

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

    IMPORT(SK, standard_debug_port, const);
    IMPORT(SK, write_to,);

  public:
    auto header(bytestring const& title = "compiler") const -> bytestring
    {
      bytestring s {"; "};

      if (not indent::depth)
      {
        s.append(title);
      }

      s.resize(18, ' ');
      s.replace(s.size() - 3, 3, " ; ");

      return s;
    }

    template <typename... Ts>
    decltype(auto) debug(Ts&&... xs)
    {
      return write_to(standard_debug_port(), header(), indent(), std::forward<decltype(xs)>(xs)..., "\n");
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_DEBUGGER_HPP
