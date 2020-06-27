#ifndef INCLUDED_MEEVAX_KERNEL_STRING_HPP
#define INCLUDED_MEEVAX_KERNEL_STRING_HPP

#include <meevax/kernel/character.hpp>

namespace meevax::kernel
{
  struct string
    : public virtual pair
  {
    // TODO REPLACE TO BOOST::LEXICAL_CAST
    operator std::string() const
    {
      std::stringstream buffer {};
      buffer << first.as<std::string>();

      for (const auto& each : second)
      {
        buffer << each.as<std::string>();
      }

      return buffer.str();
    }

    friend auto operator==(const string& lhs, const string& rhs)
    {
      return static_cast<std::string>(lhs) == static_cast<std::string>(rhs);
    }

    friend auto operator<<(std::ostream& os, const string& s)
      -> decltype(auto)
    {
      os << console::cyan << "\"" << car(s).as<std::string>();

      for (const auto& each : cdr(s))
      {
        if (each) // guard for malformed string
        {
          switch (const auto& s {each.as<std::string>()}; s[0])
          {
          case '\n': os << "\\n"; break;
          case '\t': os << "\\t"; break;

          default:
            os << s;
            break;
          }
        }
        else break;
      }

      return os << "\"" << console::reset;
    }
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_STRING_HPP

