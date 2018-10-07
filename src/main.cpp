#include <iostream>
#include <string>

#include <boost/cstdlib.hpp>

#include <meevax/lisp/evaluator.hpp>
#include <meevax/lisp/reader.hpp>
// #include <meevax/lisp/schemer.hpp>

int main()
{
  using namespace meevax;

  // lisp::schemer scheme {};
  // scheme();

  for (std::string buffer {}, continuation {}; std::cout << ">> ", std::getline(std::cin, buffer);) try
  {
    const auto well_formed_expression {lisp::read(continuation += buffer)};
    std::cout << "\n=> " << lisp::eval(well_formed_expression) << "\n\n";
    continuation.clear();
  }
  catch (const std::string& unbalance_expression)
  {
    continuation = unbalance_expression + " ";
  }

  return boost::exit_success;
}

