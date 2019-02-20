#ifndef INCLUDED_MEEVAX_SYSTEM_READER_HPP
#define INCLUDED_MEEVAX_SYSTEM_READER_HPP

#include <algorithm> // std::find_if
#include <iterator> // std::begin, std::end
#include <utility>

#include <meevax/system/cursor.hpp>
#include <meevax/system/modular.hpp>

namespace meevax::system
{
  class reader
  {
    const cursor module;

  public:
    explicit reader(const cursor& module)
      : module {module}
    {}

    void operator()(std::istream& is)
    {
      for (char buffer {is.get()}; buffer != EOF; buffer = is.get())
      {
        std::cerr << "[debug] reader: " << buffer << std::endl;
      }
    }
  };
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_READER_HPP

