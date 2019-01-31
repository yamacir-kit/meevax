#ifndef INCLUDED_MEEVAX_CORE_MACHINE_HPP
#define INCLUDED_MEEVAX_CORE_MACHINE_HPP

#include <meevax/core/cursor.hpp>

namespace meevax::core
{
  class machine
  {
    cursor s, e, c, d;
    cursor env;
  };
} // namespace meevax::core

#endif // INCLUDED_MEEVAX_CORE_MACHINE_HPP

