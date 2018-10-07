#include <fstream>
#include <iostream>
#include <list>
#include <string>
#include <utility>

#include <boost/cstdlib.hpp>

#include <meevax/lisp/evaluator.hpp>
#include <meevax/lisp/reader.hpp>

int main()
{
  using namespace meevax;

  while (true)
  {
    std::cout << "\n>> " << lisp::eval(lisp::read(std::cin, ".. ")) << std::endl;
  }

  return boost::exit_success;
}

