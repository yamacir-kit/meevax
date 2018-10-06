#include <iostream>
#include <sstream>
#include <string>
#include <utility>

#include <boost/cstdlib.hpp>

#include <meevax/lisp/evaluator.hpp>
#include <meevax/lisp/reader.hpp>

#include <meevax/debug/The_Roots_of_Lisp.hpp>

auto main()
  -> int
{
  using namespace meevax;
  using namespace meevax::debug;

  for (const auto& [source_code, evaluated] : The_Roots_of_Lisp)
  {
    std::stringstream ss {};
    ss << lisp::eval(lisp::read(source_code));

    std::cerr << "\n"
              << "source code: \e[32m" << source_code << "\e[0m\n"
              << "  -> \e[36m" << ss.str() << "\e[0m\n";

    if (evaluated == ss.str())
    {
      std::cerr << "  -> \e[1;33msuccess\e[0m\n";
    }
    else
    {
      std::cerr << "  -> \e[1;31mfailed\e[0m\n"
                << "  -> " << evaluated << " expected" << std::endl;
      std::exit(boost::exit_failure);
    }
  }

  std::cerr << "\nall tests passed." << std::endl;

  // for (std::string buffer {}; std::cout << "\n>> ", std::getline(std::cin, buffer); )
  // {
  //   if (buffer == "(exit)")
  //   {
  //     std::exit(boost::exit_success);
  //   }
  //   else
  //   {
  //     std::cout << lisp::eval(lisp::read(buffer)) << std::endl;
  //   }
  // }

  while (true)
  {
    std::cout << "\n>> " << lisp::eval(lisp::read(std::cin)) << std::endl;
  }

  return boost::exit_success;
}

