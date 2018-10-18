#include <iostream>
#include <string>

#include <boost/cstdlib.hpp>

#include <meevax/lisp/evaluator.hpp>
#include <meevax/lisp/exception.hpp>
#include <meevax/lisp/reader.hpp>
#include <meevax/lisp/writer.hpp>

int main()
{
  using namespace meevax;

  std::ios_base::sync_with_stdio(false);

  for (std::string buffer {}, continuation {}; std::getline(std::cin, buffer); ) try
  {
    if (std::empty(buffer))
    {
      continue;
    }

    const auto expression {lisp::read(continuation += buffer)};
    std::cout << "-> " << expression << std::endl;

    const auto evaluated {lisp::eval(expression)};
    std::cout << "-> " << evaluated << std::endl;

    continuation.clear();
    std::cout << std::endl;
  }
  catch (const std::string& unbalance_expression)
  {
    continuation = unbalance_expression + " ";
  }
  catch (const meevax::lisp::exception& exception)
  {
    std::cerr << exception << std::endl;
    continuation.clear();
  }

  return boost::exit_success;
}

