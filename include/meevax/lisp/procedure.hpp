define("quote", [](auto&& exp, auto)
{
  return cadr(exp);
});

define("atom", [&](auto&& exp, auto&& env)
{
  return atom(evaluate(cadr(exp), env)) ? t : nil;
});

define("eq", [&](auto&& exp, auto&& env)
{
  return evaluate(cadr(exp), env) == evaluate(caddr(exp), env) ? t : nil;
});

define("if", [&](auto&& exp, auto&& env)
{
  return evaluate(
    evaluate(cadr(exp), env) ? caddr(exp) : cadddr(exp),
    env
  );
});

define("cond", [&](auto&& exp, auto&& env)
{
  const auto buffer {
    std::find_if(cdr(exp), nil, [&](auto iter)
    {
      return evaluate(car(iter), env);
    })
  };
  return evaluate(cadar(buffer), env);
});

define("car", [&](auto&& exp, auto&& env)
{
  return car(evaluate(cadr(exp), env));
});

define("cdr", [&](auto&& exp, auto&& env)
{
  return cdr(evaluate(cadr(exp), env));
});

define("cons", [&](auto&& e, auto&& a)
{
  return evaluate(cadr(e), a) | evaluate(caddr(e), a);
});

define("lambda", [&](auto&& exp, auto&& env)
{
  using binder = utility::binder<closure, cell>;
  return std::make_shared<binder>(exp, env);
});

define("define", [&](auto&& var, auto)
{
  return assoc(
    cadr(var),
    env_ = list(cadr(var), caddr(var)) | env_
  );
});

define("list", [&](auto&& exp, auto&& env)
{
  return lambda::y([&](auto&& proc, auto&& exp, auto&& env) -> cursor
  {
    return evaluate(car(exp), env) | (cdr(exp) ? proc(proc, cdr(exp), env) : nil);
  })(cdr(exp), env);
});

define("exit", [](auto, auto)
  -> const cursor&
{
  std::exit(boost::exit_success);
});

