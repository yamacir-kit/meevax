#include <iostream>

#include <module/scheme/base.hpp>

meevax::core::cursor banner(const meevax::core::cursor&)
{
  std::cout << "Meevax Lisp" << std::endl;
  return meevax::core::unit;
}

