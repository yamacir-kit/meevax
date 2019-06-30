#include <limits>
#include <thread>

#include <boost/cstdlib.hpp>

#include <xcb/xcb.h>
#include <xcb/xproto.h>

#include <meevax/posix/linker.hpp>
#include <meevax/system/environment.hpp>
#include <meevax/utility/demangle.hpp>

#include <meevax/protocol/connection.hpp>
#include <meevax/protocol/machine.hpp>
#include <meevax/visual/context.hpp>
#include <meevax/visual/surface.hpp>

int main() try
{
  meevax::system::environment program {meevax::system::scheme_report_environment<7>};

  program.surface.map();
  program.surface.configure(XCB_CONFIG_WINDOW_WIDTH | XCB_CONFIG_WINDOW_HEIGHT, 1280u, 720u);
  program.surface.size(1280, 720);

  program.connection.flush();

  std::thread([&]()
  {
    program.surface.visualize();
  }).detach();

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
    continue; // TODO EXIT IF NOT IN INTARACTIVE MODE
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

