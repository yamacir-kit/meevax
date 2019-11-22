#ifndef INCLUDED_MEEVAX_UTILITY_HEXDUMP
#define INCLUDED_MEEVAX_UTILITY_HEXDUMP

#include <functional> // std::invoke
#include <iomanip> // std::hex,
#include <iostream> // std::ostream
#include <vector> // std::vector

namespace meevax::utility
{
  template <typename T>
  struct hexdump
  {
    const std::vector<std::uint8_t> data;

    explicit hexdump(const T& value)
      : data {
          reinterpret_cast<typename decltype(data)::const_pointer>(&value),
          reinterpret_cast<typename decltype(data)::const_pointer>(&value) + sizeof(T)
        }
    {}

    // TODO UPDATE WITH STD::ENDIAN (C++20)
    std::ostream& operator()(std::ostream& os) const
    {
      for (auto iter {std::rbegin(data)}; iter != std::rend(data); ++iter) // little endian
      // for (auto iter {std::begin(data)}; iter != std::end(data); ++iter) // big endian
      {
        os << std::setw(2) << std::setfill('0') << std::hex << static_cast<unsigned int>(*iter) << " ";
      }

      return os;
    }
  };

  template <typename T>
  std::ostream& operator<<(std::ostream& os, const hexdump<T>& hexdump)
  {
    return std::invoke(hexdump, os);
  }
} // meevax::utility

#endif // INCLUDED_MEEVAX_UTILITY_HEXDUMP

