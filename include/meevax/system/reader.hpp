#ifndef INCLUDED_MEEVAX_SYSTEM_READER_HPP
#define INCLUDED_MEEVAX_SYSTEM_READER_HPP

#include <iostream>
#include <iterator> // std::begin, std::end
#include <sstream>
#include <string>
#include <utility>

#include <meevax/string/tokenizer.hpp>
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
      string::tokenize(is);
    }
  };
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_READER_HPP

