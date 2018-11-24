#include <iostream>
#include <string>

#include <meevax/lisp/evaluator.hpp>
#include <meevax/lisp/exception.hpp>
#include <meevax/lisp/reader.hpp>
#include <meevax/lisp/writer.hpp>

int main()
{
  std::ios_base::sync_with_stdio(false);

  meevax::lisp::reader read {};
  meevax::lisp::evaluator eval {};

  for (std::string buffer {}, continuation {}; std::getline(std::cin, buffer); ) try
  {
    if (std::empty(buffer))
    {
      continue;
    }

    auto expression {read(continuation += buffer)};
    std::cout << "-> " << expression << std::endl;

    const auto evaluated {eval(expression)};
    std::cout << "-> " << evaluated << std::endl;

    continuation.clear();
    std::putchar('\n');
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

  return 0;
}

