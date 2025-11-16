#undef NDEBUG

#include <cassert>
#include <regex>
#include <typeindex>

#include <meevax/basis.hpp>
#include <meevax/kernel/boot.hpp>
#include <meevax/kernel/eof.hpp>
#include <meevax/kernel/interaction_environment.hpp>
#include <meevax/kernel/library.hpp>
#include <meevax/utility/debug.hpp>

auto root_object_counts_by_type()
{
  using namespace meevax;

  auto root_count = std::size_t(0);

  auto non_root_count = std::size_t(0);

  auto count_of = std::map<std::type_index, std::size_t>();

  for (auto const m : default_collector::mutators)
  {
    if (default_collector::is_root(m))
    {
      ++root_count;
      ++count_of[m->type()];
    }
    else
    {
      ++non_root_count;
    }
  }

  std::cerr << "ROOT:" << std::endl;

  for (auto const& [type, count] : count_of)
  {
    auto static const pattern = std::regex(R"(<.*>)");

    std::cerr << "  "
              << std::regex_replace(demangle(type.name()), pattern, "<...>")
              << " = "
              << count
              << std::endl;
  }

  return std::make_tuple(root_count, non_root_count, count_of);
}

auto main() -> int
{
  using namespace meevax;

  default_collector::threshold = std::numeric_limits<std::size_t>::max();

  assert(eof_object.is<eof>());
  assert(undefined.is<ghost>());
  assert(unspecified.is<ghost>());

  {
    auto [root_count, non_root_count, count_of] = root_object_counts_by_type();

    assert(root_count == 3);
    assert(non_root_count == 0);

    assert(count_of.size() == 2);
    assert(count_of[typeid(eof  )] == 1);
    assert(count_of[typeid(ghost)] == 2);
  }

  assert(interaction_environment().is<environment>());

  {
    auto [root_count, non_root_count, count_of] = root_object_counts_by_type();

    assert(root_count == 4);
    assert(non_root_count == 0);

    assert(count_of.size() == 3);
    assert(count_of[typeid(environment)] == 1); // The interaction-environment
    assert(count_of[typeid(eof        )] == 1);
    assert(count_of[typeid(ghost      )] == 2);
  }

  boot();

  {
    auto [root_count, non_root_count, count_of] = root_object_counts_by_type();

    assert(root_count == 516);

    assert(count_of.size() == 6);
    assert(count_of[typeid(environment                       )] ==   1); // The interaction-environment
    assert(count_of[typeid(environment::syntactic_environment)] ==   1); // The core syntactic-environment
    assert(count_of[typeid(eof                               )] ==   1);
    assert(count_of[typeid(ghost                             )] ==   2);
    assert(count_of[typeid(library                           )] ==  29); // There are 29 builtin libraries
    assert(count_of[typeid(symbol                            )] == 482); // There are 459 builtin definitions
  }

  boot(basis());

  {
    auto [root_count, non_root_count, count_of] = root_object_counts_by_type();

    assert(count_of.size() == 6);
    assert(count_of[typeid(environment                       )] ==   1); // The interaction-environment
    assert(count_of[typeid(environment::syntactic_environment)] ==   1); // The core syntactic-environment
    assert(count_of[typeid(eof                               )] ==   1);
    assert(count_of[typeid(ghost                             )] ==   2);
    assert(count_of[typeid(library                           )] ==  78);
    assert(count_of[typeid(symbol                            )] >= 482);
  }

  default_collector::collect();

  {
    auto [root_count, non_root_count, count_of] = root_object_counts_by_type();

    assert(count_of.size() == 6);
    assert(count_of[typeid(environment                       )] ==   1); // The interaction-environment
    assert(count_of[typeid(environment::syntactic_environment)] ==   1); // The core syntactic-environment
    assert(count_of[typeid(eof                               )] ==   1);
    assert(count_of[typeid(ghost                             )] ==   2);
    assert(count_of[typeid(library                           )] ==  78);
    assert(count_of[typeid(symbol                            )] >= 482); // There are 459 builtin definitions
  }

  symbols().clear();
  assert(symbols().empty());

  libraries().clear();
  assert(libraries().empty());

  const_cast<object &>(interaction_environment()).reset(); // DIRTY HACK!

  const_cast<object &>(core_syntactic_environment()).reset(); // DIRTY HACK!

  default_collector::collect();

  {
    auto [root_count, non_root_count, count_of] = root_object_counts_by_type();

    assert(count_of.size() == 2);
    assert(count_of[typeid(eof  )] == 1);
    assert(count_of[typeid(ghost)] == 2);
  }

  assert(default_collector::count() == 3); // -1 is interaction_environment

  return EXIT_SUCCESS;
}
