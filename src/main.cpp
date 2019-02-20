#include <iostream>
#include <list>
#include <string>

#include <meevax/system/compiler.hpp>
#include <meevax/system/machine.hpp>
#include <meevax/system/modular.hpp>
#include <meevax/system/reader.hpp>
// #include <meevax/posix/dynamic_link.hpp>

#include <boost/cstdlib.hpp>
#include <boost/range/algorithm.hpp>

int main()
{
  // const auto package {std::make_shared<meevax::system::namescope>(
  //   std::make_pair("nil", meevax::system::unit)
  // )};

  using namespace meevax::system;

  cursor module {cursor::bind<modular>(unit)};

  // const auto link {meevax::posix::link("/home/yamasa/works/meevax/build/libscheme-base.so")};
  // load(link, "banner").data().as<meevax::system::procedure>()(meevax::system::unit);

  meevax::system::reader read {module};
  meevax::system::compiler compile {module.as<modular>()};
  meevax::system::machine machine {module.as<modular>()};

  // TODO Initialize by contents of history file.
  // std::list<std::string> history {""};
  // std::size_t index {0};

  while (std::cin) try
  {
    read(std::cin);
  }
  // for (std::string buffer {}; std::cout << "> ", std::getline(std::cin, buffer); ) try
  // {
  //   std::string code {std::begin(buffer), boost::find(buffer, ';')};
  //   std::cerr << "\x1B[38;5;240m" << ++index << " " << buffer << "\x1B[0m" << (std::empty(buffer) ? "\r" : "\n");
  //
  //   if (auto tokens {meevax::system::tokenize<std::list>(history.back() += code)}; not std::empty(tokens) and boost::count(tokens, "(") <= boost::count(tokens, ")"))
  //   {
  //     auto expression {read(tokens)};
  //     // std::cerr << "[debug] read: " << expression << std::endl;
  //     std::cerr << expression << std::endl;
  //
  //     auto machine_code {compile(expression)};
  //     // std::cerr << "[debug] compile: " << machine_code << std::endl;
  //
  //     std::cout << machine.execute(machine_code) << "\n\n";
  //     history.emplace_back("");
  //   }
  // }
  catch (const std::runtime_error& error)
  {
    std::cerr << "\x1B[31m[error] " << error.what() << "\x1B[0m\n\n";

    // TODO stack trace or abort or exit/quit
    // history.emplace_back("");

    return boost::exit_failure;
  }

  return boost::exit_success;
}

