#include <boost/cstdlib.hpp>
#include <meevax/kernel/syntactic_continuation.hpp>

int main(const int argc, char const* const* const argv) try
{
  using meevax::let;

  auto root = meevax::syntactic_continuation(meevax::layer<4>());

  root.configure(argc, argv);

  for (auto const& each : root.paths)
  {
    root.write_to(root.standard_interaction_port(), meevax::header(__func__), "load ", each, "\n");
    root.load(each.as<meevax::path>());
  }

  if (root.in_interactive_mode())
  {
    root.write_to(root.standard_interaction_port(), meevax::header(__func__), "You have control of root syntactic-continuation.\n");

    for (auto index = 0; root.ready(); ++index)
    {
      root.write_to(root.standard_interaction_port(), "\n<< ");
      root.write_to(root.standard_interaction_port(), "\n; ", root.evaluate(root.read()), "\n");
    }

    root.write_to(root.standard_interaction_port(), "\n", meevax::header(__func__), "I have control of root syntactic-continuation.\n");
  }

  return boost::exit_success;
}


// NOTE: Perform I/O in the C++ way, as there may be a serious anomaly in Lisp system.

catch (meevax::object const& error) // is the default-exception-handler
{
  std::cerr << "error: " << error << std::endl;
  return boost::exit_exception_failure;
}

catch (meevax::error const& error)
{
  std::cerr << "system-error: " << error.what() << "." << std::endl;
  return boost::exit_exception_failure;
}

catch (std::exception const& error)
{
  std::cerr << "system-error: " << error.what() << "." << std::endl;
  return boost::exit_exception_failure;
}

catch (...)
{
  std::cerr << "system-error: An unknown object was thrown that was neither a Meevax exception type nor a C++ standard exception type." << std::endl;
  return boost::exit_exception_failure;
}

