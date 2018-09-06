#include <iostream>
#include <string>

#include <meevax/lisp/evaluator.hpp>
#include <meevax/lisp/reader.hpp>

auto main()
  -> int
{
  for (std::string buffer {}; std::cout << "\n>> ", std::getline(std::cin, buffer); )
  {
    const auto e {meevax::lisp::read(buffer)};
    std::cerr << "read: " << e << std::endl;

    const auto result {meevax::lisp::eval(e)};
    std::cerr << "eval: " << result << std::endl;
  }

  return 0;
}

