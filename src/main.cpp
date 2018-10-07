#include <fstream>
#include <iostream>
#include <list>
#include <string>
#include <utility>

#include <boost/cstdlib.hpp>

#include <meevax/lisp/evaluator.hpp>
#include <meevax/lisp/reader.hpp>

auto main(int argc, char** argv)
  -> int
{
  const std::list<std::string> args {argv + 1, argv + argc};

  using namespace meevax;

  for (const auto& each : args)
  {
    std::ifstream file {each};

    while (file && !file.eof())
    {
      std::cout << lisp::eval(lisp::read(file)) << std::endl;
    }
  }

  while (true)
  {
    std::cout << "\n>> " << lisp::eval(lisp::read(std::cin, ".. ")) << std::endl;
  }

  return boost::exit_success;
}

