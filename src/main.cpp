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

  while (std::cin) try
  {
    auto expression {read(std::cin)};
    std::cerr << "[read] " << expression << std::endl;

    auto code {compile(expression)};
    std::cerr << machine.execute(code) << "\n\n";
  }
  catch (const std::runtime_error& error)
  {
    std::cerr << "\x1B[31m[error] " << error.what() << "\x1B[0m\n\n";
    continue;
  }

  return boost::exit_success;
}

