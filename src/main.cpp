#include <chrono>
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

  program.visible();
  program.configure(XCB_CONFIG_WINDOW_WIDTH | XCB_CONFIG_WINDOW_HEIGHT, 1280u, 720u);
  program.size(1280, 720);

  program.flush();

  const std::string title {"Meevax Lisp System 0"};
  {
    meevax::visual::context context {program};
    context.set_source_rgb(0xf5 / 256.0, 0xf5 / 256.0, 0xf5 / 256.0);
    context.paint();

    context.set_source_rgb(0, 0, 0);
    context.select_font_face("Latin Modern Roman", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL);
    context.set_font_size(1280 / std::size(title) * 1.5);

    cairo_text_extents_t extents {};
    context.text_extents(title.c_str(), &extents);

    context.move_to(
      1280 * 0.5 - extents.width  * 0.5,
       720 * 0.5 + extents.height * 0.5
    );
    context.show_text(title.c_str());

    program.flush();
  }

  std::thread([&]()
  {
    program.drive();
  }).detach();

  // program.surface.update = [&](auto&& surface)
  // {
  //   meevax::visual::context context {surface};
  //   context.set_source_rgb(0xF5 / 256.0, 0xF5 / 256.0, 0xF5 / 256.0);
  //   context.paint();
  //   // visualize(surface, program.cursor);
  // };
  //
  // std::thread([&]()
  // {
  //   while (true)
  //   {
  //     program.surface.update(program.surface);
  //     program.surface.flush();
  //     std::this_thread::sleep_for(std::chrono::milliseconds {10});
  //   }
  // }).detach();

  for (program.open("/dev/stdin"); program.ready(); ) try
  {
    std::cout << "\n> " << std::flush;
    const auto expression {program.read()};
    std::cerr << "\n; read    \t; " << expression << std::endl;

    program.cursor = expression;

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

