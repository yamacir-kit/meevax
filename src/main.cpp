#include <iostream>
#include <string>

#include <meevax/core/evaluator.hpp>
#include <meevax/core/reader.hpp>
#include <meevax/core/writer.hpp>

int main()
{
  std::ios_base::sync_with_stdio(false);

  meevax::core::evaluator evaluate {};

  for (std::string buffer {}, continuation {}; std::getline(std::cin, buffer); ) try
  {
    if (std::empty(buffer))
    {
      continue;
    }

    const auto result {evaluate(continuation += buffer)};
    std::cout << "-> " << result << std::endl;

    continuation.clear();
    std::putchar('\n');
  }
  catch (const std::string& unbalance_expression)
  {
    continuation = unbalance_expression + " ";
  }
  catch (const std::runtime_error& error)
  {
    std::cerr << "[error] standard exception occurred: " << error.what() << std::endl;
    continuation.clear();
  }

  return 0;
}

