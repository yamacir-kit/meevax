#ifndef INCLUDED_MEEVAX_KERNEL_DEBUGGER_HPP
#define INCLUDED_MEEVAX_KERNEL_DEBUGGER_HPP

#include <ostream>

namespace meevax { inline namespace kernel
{
  template <typename SK>
  class debugger
  {
    friend SK;

    debugger()
    {}

    Import(SK, write_to);
    Import_Const(SK, current_debug_port);

  public:
    static inline           std::size_t depth {0};
    static inline constexpr std::size_t default_shift {2};

    auto shift() const noexcept
    {
      return default_shift;
    }

    auto header(const std::string& title = "compiler") const -> std::string
    {
      std::string s {"; "};

      if (not depth)
      {
        s.append(title);
      }

      s.resize(18, ' ');
      s.replace(s.size() - 3, 3, " ; ");

      return s;
    }

    auto indent()
    {
      return indentation();
    }

    template <typename... Ts>
    auto debug(Ts&&... xs) -> decltype(auto)
    {
      return
        write_to(current_debug_port(),
          header(), indent(), std::forward<decltype(xs)>(xs)..., "\n");
    }

    struct indentation
    {
      operator std::string() const
      {
        return std::string(depth, ' ');
      }

      friend auto operator <<(std::ostream& os, const indentation& indent)
        -> decltype(os)
      {
        return os << static_cast<std::string>(indent);
      }

      friend auto& operator >>(indentation& indent, std::size_t width)
      {
        depth += width;
        return indent;
      }

      friend auto& operator >>(indentation&& indent, std::size_t width)
      {
        depth += width;
        return indent;
      }

      friend auto& operator <<(indentation& indent, std::size_t width)
      {
        depth -= std::min(depth, width);
        return indent;
      }

      friend auto& operator <<(indentation&& indent, std::size_t width)
      {
        depth -= std::min(depth, width);
        return indent;
      }
    };
  };
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_DEBUGGER_HPP
