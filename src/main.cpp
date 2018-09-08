#include <chrono>
#include <iostream>
#include <list>
#include <sstream>
#include <string>
#include <thread>
#include <utility>

#include <boost/cstdlib.hpp>

#include <meevax/lisp/evaluator.hpp>
#include <meevax/lisp/reader.hpp>

auto main()
  -> int
{
  using namespace meevax;

  std::list<std::pair<std::string, std::string>> tests
  {
    {"(quote a)", "a"},
    {"(quote (a b c))", "(a . (b . (c . nil)))"},
    {"(atom (quote a))", "true"},
    {"(atom (quote (a b c)))", "nil"},
    {"(atom (quote ()))",  "true"},
    {"(atom (atom (quote a)))",  "true"},
    {"(atom (quote (atom (quote a))))", "nil"},
    {"(eq (quote a) (quote a))", "true"},
    {"(eq (quote a) (quote b))", "nil"},
    {"(eq (quote ()) (quote ()))", "true"},
    {"(car (quote (a b c)))", "a"},
    {"(cdr (quote (a b c)))", "(b . (c . nil))"}
  };

  for (const auto& [test, answer] : tests)
  {
    std::stringstream ss {};
    ss << lisp::eval(lisp::read(test));

    std::cerr << "\n"
              << "test: " << test << std::endl
              << "  -> " << ss.str() << std::endl
              << "  -> ";

    if (ss.str() == answer)
    {
      std::cerr << "success" << std::endl;
    }
    else
    {
      std::cerr << "failed" << std::endl
                << "expected: " << answer << std::endl;
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

