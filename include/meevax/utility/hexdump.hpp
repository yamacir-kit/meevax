#ifndef INCLUDED_MEEVAX_UTILITY_HEXDUMP
#define INCLUDED_MEEVAX_UTILITY_HEXDUMP

#include <iomanip>
#include <iostream>
#include <vector>

namespace meevax::utility
{
  // TODO Support stream manipulator

  // For safety and extensibility, we copy all data to std::vector<byte>.
  // Print data directly using reinterpret_cast will be more faster, but it is low-readability magic code.
  template <typename T>
  std::ostream& hexdump(std::ostream& os, const T& value)
  {
    std::vector<std::uint8_t> data {
      reinterpret_cast<const std::uint8_t*>(&value),
      reinterpret_cast<const std::uint8_t*>(&value) + sizeof(T)
    };

    for (const auto& each : data)
    {
      os << std::setw(2) << std::setfill('0') << std::hex << static_cast<unsigned int>(each) << (&each != &data.back() ? " " : "");
    }

    return os;
  }
} // meevax::utility

#endif // INCLUDED_MEEVAX_UTILITY_HEXDUMP

