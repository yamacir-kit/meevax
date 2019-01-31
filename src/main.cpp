#include <iostream>
#include <list>
#include <string>

#include <meevax/core/context.hpp>
#include <meevax/core/evaluator.hpp>
#include <meevax/core/reader.hpp>

#include <boost/cstdlib.hpp>
#include <boost/range/algorithm.hpp>

int main()
{
  const auto package {std::make_shared<meevax::core::context>(
    std::make_pair("nil", meevax::core::nil)
  )};

  meevax::core::reader read {package};
  meevax::core::evaluator evaluate {package};

  // TODO Initialize by contents of history file.
  std::list<std::string> history {""};

  for (std::string buffer {}; std::cout << "> ", std::getline(std::cin, buffer); ) try
  {
    std::string code {std::begin(buffer), boost::find(buffer, ';')};
    std::cerr << "\x1B[38;5;248m" << buffer << "\x1B[0m" << (std::empty(buffer) ? "\r" : "\n");

    if (auto tokens {meevax::core::tokenize<std::list>(history.back() += code)}; not std::empty(tokens) and boost::count(tokens, "(") <= boost::count(tokens, ")"))
    {
      auto expression {read(tokens)};
      std::cerr << "[debug] reader: " << expression << std::endl;

      std::cerr << evaluate(expression) << "\n\n";
      history.emplace_back("");
    }
  }
  catch (const std::runtime_error& error)
  {
    std::cerr << "[error] standard exception occurred: " << error.what() << "\n\n";
    history.emplace_back("");
  }

  return boost::exit_success;
}

