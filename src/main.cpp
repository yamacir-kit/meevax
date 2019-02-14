#include <iostream>
#include <list>
#include <string>

#include <meevax/core/compiler.hpp>
#include <meevax/core/context.hpp>
#include <meevax/core/machine.hpp>
#include <meevax/core/reader.hpp>
#include <meevax/posix/dynamic_link.hpp>

#include <boost/cstdlib.hpp>
#include <boost/range/algorithm.hpp>

int main()
{
  const auto package {std::make_shared<meevax::core::context>(
    std::make_pair("nil", meevax::core::unit)
  )};

  auto handle {meevax::posix::dynamic_link("/home/yamasa/works/meevax/build/libmeevax-base.so")};
  void* pointer {dlsym(handle.get(), "banner")};

  if (pointer)
  {
    meevax::core::procedure func {
      "banner",
      reinterpret_cast<meevax::core::procedure::signature>(pointer)
    };
    func(meevax::core::unit);
  }
  else
  {
    std::cerr << dlerror() << std::endl;
    std::exit(boost::exit_failure);
  }

  meevax::core::reader read {package};
  meevax::core::compiler compile {package};
  meevax::core::machine machine {package};

  // TODO Initialize by contents of history file.
  std::list<std::string> history {""};
  std::size_t index {0};

  for (std::string buffer {}; std::cout << "> ", std::getline(std::cin, buffer); ) try
  {
    std::string code {std::begin(buffer), boost::find(buffer, ';')};
    std::cerr << "\x1B[38;5;240m" << ++index << " " << buffer << "\x1B[0m" << (std::empty(buffer) ? "\r" : "\n");

    if (auto tokens {meevax::core::tokenize<std::list>(history.back() += code)}; not std::empty(tokens) and boost::count(tokens, "(") <= boost::count(tokens, ")"))
    {
      auto expression {read(tokens)};
      // std::cerr << "[debug] read: " << expression << std::endl;
      std::cerr << expression << std::endl;

      auto machine_code {compile(expression)};
      // std::cerr << "[debug] compile: " << machine_code << std::endl;

      std::cout << machine.execute(machine_code) << "\n\n";
      history.emplace_back("");
    }
  }
  catch (const std::runtime_error& error)
  {
    std::cerr << "\x1B[31m[error] " << error.what() << "\x1B[0m\n\n";

    // TODO stack trace or abort or exit/quit
    // history.emplace_back("");

    return boost::exit_failure;
  }

  return boost::exit_success;
}

