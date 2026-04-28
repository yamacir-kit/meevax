#include <meevax/kernel/environment.hpp>
#include <meevax/kernel/procedure.hpp>

namespace meevax::inline example
{
  auto argument_length(object const& xs)
  {
    return make(static_cast<small_integer>(length(xs)));
  }

  auto dummy_procedure(object const& xs)
  {
    std::cout << "\n; calling C++ function." << std::endl;

    std::size_t count = 0;

    for (let const& each : xs)
    {
      std::cout << "; [" << count++ << "] is " << each << " (C++ typename " << each.type().name() << ")" << std::endl;
    }

    for (let const& x : xs)
    {
      if (x.is<small_integer>())
      {
        std::cout << "; return incremented left-most integer object." << std::endl;

        return make(x.as<small_integer>() + 1);
      }
    }

    return unspecified;
  }

  struct hoge
  {
    small_integer value;

    explicit hoge(small_integer value)
      : value { value }
    {}

    ~hoge()
    {
      std::cout << "DESTRUCTOR!" << std::endl;
    }
  };

  auto make_hoge(object const& xs)
  {
    return make<hoge>(exact_integer_cast<small_integer>(car(xs)));
  }

  auto hoge_value(object const& xs)
  {
    return make<small_integer>(car(xs).as<hoge>().value);
  }

  extern "C"
  {
    auto lookup(char const* name) -> void *
    {
      auto static const registry = std::unordered_map<std::string, meevax::procedure::signature>
      {
        { "argument_length", argument_length },
        { "dummy_procedure", dummy_procedure },
        { "make_hoge", make_hoge },
        { "is_hoge", [](object const& xs) { return make(car(xs).is<hoge>()); } },
        { "hoge_value", hoge_value }
      };

      if (auto iterator = registry.find(name); iterator != registry.end())
      {
        return reinterpret_cast<void *>(iterator->second);
      }
      else
      {
        return nullptr;
      }
    }
  }
}
