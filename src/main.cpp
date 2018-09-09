#include <chrono>
#include <iostream>
#include <sstream>
#include <string>
#include <thread>
#include <utility>

#include <boost/cstdlib.hpp>

#include <meevax/lisp/evaluator.hpp>
#include <meevax/lisp/reader.hpp>

#include <meevax/debug/The_Roots_of_Lisp.hpp>

auto main()
  -> int
{
  using namespace meevax;

  for (const auto& [test, answer] : meevax::debug::The_Roots_of_Lisp)
  {
    std::stringstream ss {};
    ss << lisp::eval(lisp::read(test));

    std::cerr << "\ntest: \e[32m" << test << "\e[0m\n"
              << "  -> \e[36m" << ss.str() << "\e[0m\n";

    if (answer == "lambda" || ss.str() == answer)
    {
      std::cerr << "  -> \e[1;33msuccess" << "\e[0m\n";
    }
    else
    {
      std::cerr << "  -> \e[1;31mfailed" << "\e[0m\n"
                << "  -> " << answer << " expected" << std::endl;
      std::exit(boost::exit_failure);
    }

    std::this_thread::sleep_for(std::chrono::milliseconds {100});
  }

  std::cerr << "\nall tests passed." << std::endl;

  for (std::string buffer {}; std::cout << "\n>> ", std::getline(std::cin, buffer); )
  {
    std::cout << lisp::eval(lisp::read(buffer)) << std::endl;
  }

  return boost::exit_success;
}

