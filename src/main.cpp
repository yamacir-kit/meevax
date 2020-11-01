// #define THE_ONLY_SUBSET_OF_THE_EMPTY_SET_IS_ITSELF true

#define MEEVAX_USE_GMP

#include <meevax/kernel/syntactic_continuation.hpp>

int main(const int argc, char const* const* const argv) try
{
  using namespace meevax::kernel;

  meevax::kernel::syntactic_continuation root { meevax::kernel::layer<4>() };

  // TODO pass argc and argv to syntactic-continuation's constructor directly.
  root.configure(argc, argv);

  for (const auto& each : root.paths)
  {
    root.write_to(
      root.current_debug_port(),
      root.header("overture"), "load ", each, "\n");

    root.load(each.as<path>());
  }

  // TODO exit if not in interactive_mode
  if (root.in_interactive_mode())
  {
    root.write_to(
      root.current_interaction_port(),
      root.header("interaction"), "You have control of root syntactic-continuation.\n");

    for (const auto prompt { "\n> " }; root.ready(); ) try
    {
      root.write_to(
        root.current_interaction_port(), prompt);

      const auto expression { root.read() };

      root.write_to(
        root.current_interaction_port(), "\n");

      root.write_to(
        root.current_debug_port(),
        root.header("read"), expression, "\n");

      const auto evaluation { root.evaluate(expression) };

      root.write_to(
        root.current_interaction_port(),
        evaluation, "\n");
    }
    catch (const meevax::kernel::object& something) // runtime exception generated by user code
    {
      std::cerr << something << std::endl; // NOTE: Use std::cerr directly because the meevax may be broken.
      continue;
    }
    catch (const meevax::kernel::error& datum)
    {
      std::cerr << datum << std::endl; // NOTE: Use std::cerr directly because the meevax may be broken.

      if (root.in_interactive_mode())
      {
        continue;
      }
      else
      {
        return boost::exit_exception_failure;
      }
    }

    root.write_to(
      root.current_interaction_port(),
      "\n",
      root.header("interaction"), "I have control of root syntactic-continuation.\n");
  }

  return boost::exit_success;
}
catch (const meevax::kernel::error& datum)
{
  std::cerr << datum << std::endl;
  return boost::exit_exception_failure;
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

