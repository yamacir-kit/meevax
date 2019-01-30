#include <iostream>
#include <string>

#include <meevax/core/context.hpp>
#include <meevax/core/evaluator.hpp>
#include <meevax/core/reader.hpp>

int main()
{
  std::ios_base::sync_with_stdio(false);

  auto package {std::make_shared<meevax::core::context>()};

  meevax::core::evaluator evaluate {package};

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

