#include <chrono>
#include <iostream>
#include <list>
#include <sstream>
#include <string>
#include <thread>

#include <boost/cstdlib.hpp>
#include <boost/range/combine.hpp>

#include <meevax/lisp/evaluator.hpp>
#include <meevax/lisp/reader.hpp>

auto main()
  -> int
{
  using namespace meevax;

  std::list<std::string> tests {
    "(quote a)",
    "(quote (a b c))"
  };

  std::list<std::string> answers {
    "a",
    "(a . (b . (c . nil)))"
  };

  for (const auto& tuple : boost::combine(tests, answers))
  {
    std::stringstream ss {};
    ss << lisp::eval(lisp::read(boost::get<0>(tuple)));

    std::cerr << "\n"
              << "test: " << boost::get<0>(tuple) << std::endl
              << "  -> " << ss.str() << std::endl
              << "  -> ";

    if (ss.str() == boost::get<1>(tuple))
    {
      std::cerr << "success" << std::endl;
    }
    else
    {
      std::cerr << "failed" << std::endl;
      std::exit(boost::exit_failure);
    }

    std::this_thread::sleep_for(std::chrono::seconds {1});
  }

  std::cerr << "\n"
            << "all tests passed." << std::endl;

  for (std::string buffer {}; std::cout << "\n>> ", std::getline(std::cin, buffer); )
  {
    std::cout << lisp::eval(lisp::read(buffer)) << std::endl;
  }

  return boost::exit_success;
}

