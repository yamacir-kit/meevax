#include <meevax/system/module.hpp>

#include <boost/cstdlib.hpp>

int main()
{
  using namespace meevax::system;

  std::cerr << "starting boot seqence.\n" << std::endl;

  module root {unit};

  meevax::posix::linker library {"/home/yamasa/works/meevax/build/libscheme-base.so"};
  {
    root.define<special>("quote", [&](auto&& exp, auto&&, auto&& continuation)
    {
      return cons(LDC, cadr(exp), continuation);
    });

    root.define<special>("car", [&](auto&& exp, auto&& scope, auto&& continuation)
    {
      return root.compile(
               cadr(exp),
               scope,
               cons(CAR, continuation)
             );
    });

    root.define<special>("cdr", [&](auto&& exp, auto&& scope, auto&& continuation)
    {
      return root.compile(
               cadr(exp),
               scope,
               cons(CDR, continuation)
             );
    });

    root.define<special>("cons", [&](auto&& exp, auto&& scope, auto&& continuation)
    {
      return root.compile(
               caddr(exp),
               scope,
               root.compile(cadr(exp), scope, cons(CONS, continuation))
             );
    });

    root.define<special>("if", [&](auto&& exp, auto&& scope, auto&& continuation)
    {
      return root.compile(
               cadr(exp), // conditional expression
               scope,
               cons(
                 SELECT,
                 root.compile( caddr(exp), scope, list(JOIN)), // then expression
                 root.compile(cadddr(exp), scope, list(JOIN)), // else expression
                 continuation
               )
             );
    });

    root.define<special>("define", [&](auto&& exp, auto&& scope, auto&& continuation)
    {
      return root.compile(
               caddr(exp),
               scope,
               cons(DEFINE, cadr(exp), continuation)
             );
    });

    root.define<special>("lambda", [&](auto&& exp, auto&& scope, auto&& continuation)
    {
      return cons(
               LDF,
               root.begin(
                 cddr(exp),
                 cons(
                   cadr(exp), // parameters
                   scope
                 ),
                 list(RETURN)
               ),
               continuation
             );
    });

    root.define<special>("syntax", [&](auto&& exp, auto&& scope, auto&& continuation)
    {
      return cons(
               LDS,
               root.begin(
                 cddr(exp),
                 cons(
                   cadr(exp), // parameters
                   scope
                 ),
                 list(RETURN)
               ),
               continuation
             );
    });

    root.define<special>("set!", [&](auto&& exp, auto&& scope, auto&& continuation)
    {
      if (!exp)
      {
        throw error {"setting to unit"};
      }
      else if (auto location {root.execute.locate(cadr(exp), scope)}; location)
      {
        return root.compile(
                 caddr(exp),
                 scope,
                 cons(SETL, location, continuation)
               );
      }
      else
      {
        return root.compile(
                 caddr(exp),
                 scope,
                 cons(SETG, cadr(exp), continuation)
               );
      }
    });

    // XXX DIRTY HACK
    root.define<procedure>("load", [&](const cursor& args)
    {
      // XXX 今は雑にブーリアンを返してる
      return root.load(car(args).template as<const string>());
    });

    root.define<procedure>("display", [&](auto&& args)
    {
      for (auto each : args)
      {
        if (each.template is<string>()) // XXX DIRTY HACK
        {
          std::cout << static_cast<std::string>(each.template as<string>());
        }
        else
        {
          std::cout << each;
        }
      }
      return unit; // XXX DIRTY HACK
    });

    root.define<procedure>("emergency-exit", [&](auto&& args)
    {
      if (not args or not car(args).template is<number>())
      {
        std::exit(boost::exit_success);
      }
      else
      {
        // XXX DIRTY HACK
        std::exit(static_cast<int>(car(args).template as<number>()));
      }

      return unit; // XXX DIRTY HACK
    });

    root.define<procedure>("link-procedure", [&](auto&& args)
    {
      return make<procedure>(
               "unknown",
               root.link<procedure>(
                 car(args).template as<string>(),
                 cadr(args).template as<string>()
               )
             );
    });

    root.define<procedure>("eq?", library.link<procedure::signature>("eq"));
    root.define<procedure>("pair?", library.link<procedure::signature>("is_pair"));

    root.define<procedure>("*", library.link<procedure::signature>("multiply"));
    root.define<procedure>("+", library.link<procedure::signature>("plus"));
    root.define<procedure>("-", library.link<procedure::signature>("minus"));
    root.define<procedure>("/", library.link<procedure::signature>("divide"));
  }

  std::cerr << "\n"
            << "\twelcome, wizard\n"
            << "\n"
            << "you have control" << std::endl;

  for (root.open("/dev/stdin"); root.ready(); ) try
  {
    std::cerr << "\n> " << std::flush;
    const auto expression {root.read()};
    std::cerr << "\n; " << expression << std::endl;

    const auto executable {root.compile(expression)};
    std::cerr << "; as " << executable << std::endl;

    const auto evaluation {root.execute(executable)};
    std::cerr << "; => " << std::flush;
    std::cout << evaluation << std::endl;
  }
  catch (const cursor& something) // runtime exception generated by user code
  {
    std::cerr << something << std::endl;
    continue;
  }
  catch (const warning& warning)
  {
    std::cerr << warning << std::endl;
    continue;
  }
  catch (const exception& error) // runtime exception generated by core system
  {
    std::cerr << error << std::endl;
    continue; // TODO EXIT IF IN NON-INTARACTIVE MODE
  }
  catch (const std::exception& error)
  {
    std::cerr << "\x1b[1;31m"
              << "unexpected standard exception: \"" << error.what() << "\"\n"
              << "this maybe implementation bug. report this to developer.\n"
              << "shutting down meevax system.\n"
              << "\x1b[0m";
    return boost::exit_exception_failure;
  }

  return boost::exit_success;
}

