#include <meevax/system/module.hpp>

#include <boost/cstdlib.hpp>

int main()
{
  using namespace meevax::system;

  module root {"root"};

  meevax::posix::linker library {"/home/yamasa/works/meevax/build/libscheme-base.so"};
  {
    root.define<syntax>("quote", [&](auto&& exp, auto&&, auto&& continuation)
    {
      return cons(LDC, cadr(exp), continuation);
    });

    root.define<syntax>("car", [&](auto&& exp, auto&& scope, auto&& continuation)
    {
      return root.compile(
               cadr(exp),
               scope,
               cons(CAR, continuation)
             );
    });

    root.define<syntax>("cdr", [&](auto&& exp, auto&& scope, auto&& continuation)
    {
      return root.compile(
               cadr(exp),
               scope,
               cons(CDR, continuation)
             );
    });

    root.define<syntax>("cons", [&](auto&& exp, auto&& scope, auto&& continuation)
    {
      return root.compile(
               caddr(exp),
               scope,
               root.compile(cadr(exp), scope, cons(CONS, continuation))
             );
    });

    root.define<syntax>("if", [&](auto&& exp, auto&& scope, auto&& continuation)
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

    root.define<syntax>("define", [&](auto&& exp, auto&& scope, auto&& continuation)
    {
      return root.compile(
               caddr(exp),
               scope,
               cons(DEFINE, cadr(exp), continuation)
             );
    });

    root.define<syntax>("lambda", [&](auto&& exp, auto&& scope, auto&& continuation)
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

    // XXX DIRTY HACK
    root.define<procedure>("load", [&](const cursor& args)
    {
      root.load(car(args).template as<const string>());
      return args;
    });

    root.define<procedure>("eq?", library.link<procedure::signature>("eq"));
    root.define<procedure>("pair?", library.link<procedure::signature>("is_pair"));

    root.define<procedure>("*", library.link<procedure::signature>("multiply"));
    root.define<procedure>("+", library.link<procedure::signature>("plus"));
    root.define<procedure>("-", library.link<procedure::signature>("minus"));
    root.define<procedure>("/", library.link<procedure::signature>("divide"));
  }

  std::cerr << "\n"
            << "\tWelcome, wizard.\n"
            << std::endl;

  for (root.open("/dev/stdin"); root.readable() && std::cerr << "\nstandard-input ready.\n> "; ) try
  {
    const auto expression {root.read()};
    std::cerr << "\r\x1b[K> " << expression << std::endl;

    const auto executable {root.compile(expression)};
    std::cerr << "; " << executable << std::endl;

    const auto evaluation {root.execute(executable)};
    std::cerr << "; " << evaluation << std::endl;
  }
  catch (const cursor& something)
  {
    if (something && something.is<exception>())
    {
      std::cerr << something << std::endl;
      continue;
    }
    else
    {
      std::cerr << "\x1b[31m[debug] unexpected object thrown: " << something << std::endl;
      return boost::exit_exception_failure;
    }
  }
  catch (const std::runtime_error& error)
  {
    std::cerr << "\x1b[31m[fatal] " << error.what() << "\x1B[0m\n\n";
    continue;
  }

  return boost::exit_success;
}

