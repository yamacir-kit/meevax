#include <meevax/kernel/syntactic_continuation.hpp>

int main(const int argc, char const* const* const argv) try
{
  auto root = meevax::syntactic_continuation(meevax::layer<4>());

  root.configure(argc, argv);

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

catch (int exit_code)
{
  return exit_code;
}

/* ---- NOTE -------------------------------------------------------------------
 *
 *  Exceptions thrown by the run-time raise procedure reach here via the
 *  default-exception-handler (procedure '%throw'). Except procedure '%throw',
 *  any processing in namespace meevax::kernel must not throw meevax::object
 *  type object.
 *
 *  Perform I/O in the C++ way, as there may be a serious anomaly in system.
 *
 * -------------------------------------------------------------------------- */
catch (meevax::let const& error)
{
  std::cerr << meevax::header("exception-handler") << error << std::endl;
  std::cerr << meevax::header("exception-handler") << "Terminate the program without running any outstanding dynamic-wind after procedures." << std::endl;

  return boost::exit_exception_failure;
}

/* ---- NOTE -------------------------------------------------------------------
 *
 *  Exceptions thrown from the Meevax system itself will eventually reach here.
 *
 * -------------------------------------------------------------------------- */
catch (meevax::error const& error)
{
  std::cerr << meevax::header("system-error") << error.what() << "." << std::endl;

  return boost::exit_exception_failure;
}

catch (std::exception const& error)
{
  std::cerr << meevax::header("system-error") << error.what() << "." << std::endl;

  return boost::exit_exception_failure;
}

catch (...)
{
  std::cerr << meevax::header("unknown-error") << "An unknown object was thrown that was neither a Meevax exception type nor a C++ standard exception type." << std::endl;

  return boost::exit_exception_failure;
}

