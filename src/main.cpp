#include <numeric> // std::accumulate

#include <meevax/system/module.hpp>

#include <boost/cstdlib.hpp>

int main()
{
  using namespace meevax::system;

  module root {"root"};

  meevax::posix::linker library {"/home/yamasa/works/meevax/build/libscheme-base.so"};

  // XXX TEMPORARY
  // std::cerr << "loading module \"scheme-base\" into module \"root\" => ";
  {
    // root.define<syntax>("quote", [&](auto&& exp, auto&&, auto&& continuation)
    // {
    //   return cons(LDC, cadr(exp), continuation);
    // });

    root.define<syntax>("quote", library.link<syntax::signature>("quote"));

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

    root.define<procedure>("pair?", [&](const cursor& args)
    {
      for (const cursor& each : args)
      {
        if (not each or not each.is<pair>())
        {
          return false_v;
        }
      }

      return true_v;
    });

    root.define<procedure>("eq?", [&](const cursor& args)
    {
      return car(args) == cadr(args) ? true_v : false_v;
    });

    root.define<procedure>("+", [&](const cursor& args)
    {
      return std::accumulate(args, unit, make<number>(0), std::plus {});
    });

    root.define<procedure>("*", [&](const cursor& args)
    {
      return std::accumulate(args, unit, make<number>(1), std::multiplies {});
    });

    root.define<procedure>("-", [&](const cursor& args)
    {
      // TODO LENGTH
      if (std::distance(args, unit) < 2)
      {
        return std::accumulate(args, unit, make<number>(0), std::minus {});
      }
      else
      {
        return std::accumulate(cursor {cdr(args)}, unit, car(args), std::minus {});
      }
    });

    root.define<procedure>("/", [&](const cursor& args)
    {
      return std::accumulate(args, unit, make<number>(1), std::divides {});
    });
  }
  // std::cerr << "done." << std::endl;

  std::cerr << "\n"
            << "\tWelcome, wizard." << std::endl;

  std::cerr << "\nentering interactive mode." << std::endl;
  std::cerr << "opening \"/dev/stdin\"... ";

  for (root.open("/dev/stdin") && std::cerr << "done." << std::endl;
       root.readable() && std::cerr << "\nstandard-input ready.\n> "; ) try
  {
    const auto expression {root.read()};

    std::cerr << "\n";
    std::cerr << "[read] " << expression << std::endl;

    const auto code {root.compile(expression)};
    std::cerr << "[compile] " << code << std::endl;

    const auto result {root.execute(code)};
    std::cerr << "[execute] " << result << std::endl;
  }
  catch (const std::runtime_error& error)
  {
    std::cerr << "\x1B[31m[error] " << error.what() << "\x1B[0m\n\n";
    continue;
  }

  return boost::exit_success;
}

