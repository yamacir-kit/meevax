#ifndef INCLUDED_MEEVAX_KERNEL_STRING_HPP
#define INCLUDED_MEEVAX_KERNEL_STRING_HPP

#include <meevax/kernel/character.hpp>
#include <meevax/kernel/pair.hpp>
#include <meevax/kernel/port.hpp>

namespace meevax
{
inline namespace kernel
{
  struct string
    : public virtual pair
  {
    output_port& display_to(output_port& port) const
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
      output_string_port port {};
      display_to(port);
      return port.str();
    }
  };

  bool operator==(const string& lhs, const string& rhs);

  output_port& operator <<(output_port& port, const string& datum);
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_STRING_HPP
