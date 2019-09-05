#include <vector>

#include <boost/cstdlib.hpp>

#include <meevax/system/environment.hpp>

int main(const int argc, char const* const* const argv) try
{
  const std::vector<std::string> args {argv + 1, argv + argc};

  for (auto iter {std::begin(args)}; iter != std::end(args); ++iter) [&]()
  {
    using namespace meevax::system;

    if (*iter == "-v")
    {
      std::cout << configuration::version << std::endl;
      std::exit(boost::exit_success);
    }

    if (*iter == "--version")
    {
      std::cout << "; Meevax Lisp System " << configuration::version_major << " - Revision " << configuration::version_minor << " Patch " << configuration::version_patch << std::endl;
      std::cout << ";" << std::endl;
      std::cout << "; version   \t; " << configuration::version    << std::endl;
      std::cout << "; build-date\t; " << configuration::build_date << std::endl;
      std::cout << "; build-hash\t; " << configuration::build_hash << std::endl;
      std::cout << "; build-type\t; " << configuration::build_type << std::endl;
      std::cout << ";" << std::endl;
      std::cout << "; install-prefix\t; " << configuration::install_prefix << std::endl;

      std::exit(boost::exit_success);
    }

    std::cerr << "; configure\t; unknown option \"" << *iter << "\"" << std::endl;
    std::exit(boost::exit_failure);
  }();

  meevax::system::environment program {meevax::system::standard_environment<0>};

  for (program.open("/dev/stdin"); program.ready(); ) try
  {
    std::cout << "\n> " << std::flush;
    const auto expression {program.read()};
    std::cerr << "\n; read    \t; " << expression << std::endl;

    const auto executable {program.compile(expression)};

    const auto evaluation {program.execute(executable)};
    std::cerr << "; => " << std::flush;
    std::cout << evaluation << std::endl;
  }
  catch (const meevax::system::object& something) // runtime exception generated by user code
  {
    std::cerr << something << std::endl;
    continue;
  }
  catch (const meevax::system::exception& exception) // TODO REMOVE THIS
  {
    std::cerr << exception << std::endl;
    continue; // TODO EXIT IF NOT IN INTERACTIVE MODE
    // return boost::exit_exception_failure;
  }

  return boost::exit_success;
}
catch (const std::exception& error)
{
  std::cout << "\x1b[1;31m" << "unexpected standard exception: \"" << error.what() << "\"" << "\x1b[0m" << std::endl;
  return boost::exit_exception_failure;
}
catch (...)
{
  std::cout << "\x1b[1;31m" << "unexpected exception occurred." << "\x1b[0m" << std::endl;
  return boost::exit_exception_failure;
}

