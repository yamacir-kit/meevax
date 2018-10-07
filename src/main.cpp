#include <iostream>
#include <string>

#include <boost/cstdlib.hpp>

#include <meevax/lisp/evaluator.hpp>
#include <meevax/lisp/reader.hpp>

int main()
{
  using namespace meevax;

  for (std::string buffer {}, continuation {}; std::cout << ">> ", std::getline(std::cin, buffer);) try
  {
    const auto result {lisp::eval(lisp::read(continuation += buffer))};
    std::cout << "\n" << result << "\n\n";
    continuation.clear();
  }
  catch (const std::string& unbalance_expression)
  {
    continuation = unbalance_expression + " ";
  }

  return boost::exit_success;
}

