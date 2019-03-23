#include <iostream>

#include <meevax/module/scheme/base.hpp>

meevax::system::cursor banner(const meevax::system::cursor&)
{
  std::cout << "Meevax Lisp" << std::endl;
  return meevax::system::unit;
}

