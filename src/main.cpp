#include <iostream>
#include <string>

#include <boost/cstdlib.hpp>

#include <meevax/lisp/evaluator.hpp>
#include <meevax/lisp/reader.hpp>
// #include <meevax/lisp/schemer.hpp>

int main()
{
  using namespace meevax;

  std::ios_base::sync_with_stdio(false);

  // lisp::schemer scheme {};
  // scheme();

  for (std::string buffer {}, continuation {}; std::getline(std::cin, buffer); ) try
  {
    const auto well_formed_expression {lisp::read(continuation += buffer)};

    std::cout << "-> " << well_formed_expression << std::endl;
    std::cout << "-> " << lisp::eval(well_formed_expression) << std::endl;

    continuation.clear();
    std::cout << std::endl;
  }
  catch (const std::string& unbalance_expression)
  {
    continuation = unbalance_expression + " ";
  }

  return boost::exit_success;
}

