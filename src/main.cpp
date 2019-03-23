#include <iostream>
#include <stdexcept>

#include <meevax/system/compiler.hpp>
#include <meevax/system/machine.hpp>
#include <meevax/system/modular.hpp>
#include <meevax/system/reader.hpp>

#include <boost/cstdlib.hpp>

int main()
{
  using namespace meevax::system;

  cursor module {cursor::bind<modular>(unit)};

  reader read {module};
  compiler compile {module.as<modular>()};
  machine machine {module.as<modular>()};

  // TODO Initialize by contents of history file.
  // std::list<std::string> history {""};

  while (std::cin) try
  {
    auto expression {read(std::cin)};
    std::cerr << expression << std::endl;

    auto code {compile(expression)};
    std::cerr << machine.execute(code) << std::endl;
  }
  catch (const std::runtime_error& error)
  {
    std::cerr << "\x1B[31m[error] " << error.what() << "\x1B[0m\n\n";

    // TODO stack trace or abort or exit/quit
    // history.emplace_back("");

    return boost::exit_failure;
  }

  return boost::exit_success;
}

