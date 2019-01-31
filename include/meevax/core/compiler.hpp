#ifndef INCLUDED_MEEVAX_CORE_COMPILER_HPP
#define INCLUDED_MEEVAX_CORE_COMPILER_HPP

#include <functional>
#include <iostream>
#include <memory>
#include <string>
#include <unordered_map>

#include <meevax/core/instruction.hpp>
#include <meevax/core/pair.hpp>
#include <meevax/core/context.hpp>

namespace meevax::core
{
  class compiler
  {
    std::unordered_map<
      std::shared_ptr<pair>,
      std::function
    > forms;

  public:
    explicit compiler(const std::shared_ptr<context>& package)
    {
    }
  };
} // namespace meevax::core

#endif // INCLUDED_MEEVAX_CORE_COMPILER_HPP

