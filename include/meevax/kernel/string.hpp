#ifndef INCLUDED_MEEVAX_KERNEL_STRING_HPP
#define INCLUDED_MEEVAX_KERNEL_STRING_HPP

#include <meevax/kernel/character.hpp>

namespace meevax { inline namespace kernel
{
  struct string
    : public virtual pair
  {
    auto display_to(std::ostream& port) const -> decltype(auto)
    {
      car(*this).as<character>().display_to(port);

      for (const auto& each : cdr(*this))
      {
        each.as<character>().display_to(port);
      }

      return port;
    }

    auto display_to(let const& maybe_port) const -> decltype(auto)
    {
      return display_to(maybe_port.as<output_port>());
    }

    operator std::string() const
    {
      std::stringstream port {};
      display_to(port);
      return port.str();
    }

    friend auto operator==(const string& lhs, const string& rhs)
    {
      return static_cast<std::string>(lhs) == static_cast<std::string>(rhs);
    }

    friend auto operator<<(std::ostream& port, const string& s) -> decltype(auto)
    {
      port << cyan << "\"" << car(s).as<character>().display();

      for (const auto& each : cdr(s))
      {
        if (each) // guard for malformed string
        {
          switch (const auto& s { each.as<character>().display() }; s[0])
          {
          case '\n': port << "\\n"; break;
          case '\t': port << "\\t"; break;

          default:
            port << s;
            break;
          }
        }
        else break;
      }

      return port << "\"" << reset;
    }
  };
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_STRING_HPP
