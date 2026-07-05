#undef NDEBUG

#include <cassert>
#include <regex>
#include <typeindex>

#include <meevax/kernel/boot.hpp>
#include <meevax/kernel/configuration.hpp>
#include <meevax/kernel/eof.hpp>
#include <meevax/kernel/interaction_environment.hpp>
#include <meevax/kernel/library.hpp>
#include <meevax/kernel/symbol.hpp>
#include <meevax/utility/debug.hpp>

auto main() -> int
{
  using namespace meevax;

  reserve(std::numeric_limits<std::size_t>::max());

  assert(eof_object.is<eof>());
  assert(undefined.is<ghost>());
  assert(unspecified.is<ghost>());

  {
    auto status = meevax::status();

    std::cout << status << std::endl;

    assert(status.root_count == 3);
    assert(status.root_count_of.size() == 2);
    assert(status.root_count_of[typeid(eof  )] == 1);
    assert(status.root_count_of[typeid(ghost)] == 2);
    assert(status.non_root_count == 0);
  }

  assert(interaction_environment().is<environment>());

  {
    auto status = meevax::status();

    std::cout << status << std::endl;

    assert(status.root_count == 4);
    assert(status.root_count_of.size() == 3);
    assert(status.root_count_of[typeid(environment)] == 1); // The interaction-environment
    assert(status.root_count_of[typeid(eof        )] == 1);
    assert(status.root_count_of[typeid(ghost      )] == 2);
    assert(status.non_root_count == 0);
  }

  boot();

  {
    auto status = meevax::status();

    std::cout << status << std::endl;

    assert(status.root_count == 515);
    assert(status.root_count_of.size() == 6);
    assert(status.root_count_of[typeid(environment                       )] ==   1); // The interaction-environment
    assert(status.root_count_of[typeid(environment::syntactic_environment)] ==   1); // The core syntactic-environment
    assert(status.root_count_of[typeid(eof                               )] ==   1);
    assert(status.root_count_of[typeid(ghost                             )] ==   2);
    assert(status.root_count_of[typeid(library                           )] ==  27); // There are 27 primitive libraries
    assert(status.root_count_of[typeid(symbol                            )] == 483); // There are 483 builtin definitions
  }

  interaction_environment().as<environment>().load_scheme_libraries();

  {
    auto status = meevax::status();

    std::cout << status << std::endl;

    assert(status.root_count == 1431);
    assert(status.root_count_of.size() == 6);
    assert(status.root_count_of[typeid(environment                       )] ==   1); // The interaction-environment
    assert(status.root_count_of[typeid(environment::syntactic_environment)] ==   1); // The core syntactic-environment
    assert(status.root_count_of[typeid(eof                               )] ==   1);
    assert(status.root_count_of[typeid(ghost                             )] ==   2);
    assert(status.root_count_of[typeid(library                           )] ==  76);
    assert(status.root_count_of[typeid(symbol                            )] >= 482);
  }

  collect();

  {
    auto status = meevax::status();

    std::cout << status << std::endl;

    assert(status.root_count_of.size() == 6);
    assert(status.root_count_of[typeid(environment                       )] ==   1); // The interaction-environment
    assert(status.root_count_of[typeid(environment::syntactic_environment)] ==   1); // The core syntactic-environment
    assert(status.root_count_of[typeid(eof                               )] ==   1);
    assert(status.root_count_of[typeid(ghost                             )] ==   2);
    assert(status.root_count_of[typeid(library                           )] ==  76);
    assert(status.root_count_of[typeid(symbol                            )] >= 483); // There are 483 builtin definitions
  }

  symbols().clear();
  assert(symbols().empty());

  libraries().clear();
  assert(libraries().empty());

  const_cast<object &>(interaction_environment()).reset(); // DIRTY HACK!

  const_cast<object &>(core_syntactic_environment()).reset(); // DIRTY HACK!

  collect();

  {
    auto status = meevax::status();

    std::cout << status << std::endl;

    assert(status.root_count_of.size() == 2);
    assert(status.root_count_of[typeid(eof  )] == 1);
    assert(status.root_count_of[typeid(ghost)] == 2);
    assert(status.non_root_count == 0);
  }

  assert(count() == 3); // -1 is interaction_environment

  return EXIT_SUCCESS;
}
