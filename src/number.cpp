#include <meevax/kernel/number.hpp>

namespace meevax
{
inline namespace kernel
{
  [[deprecated]]
  auto to_inexact(const exact_integer& datum) -> default_float
  {
    return default_float(datum.value.convert_to<default_float::value_type>());
  }

  [[deprecated]]
  auto to_inexact(const ratio& datum) -> default_float
  {
    return to_inexact(datum.numerator().as<exact_integer>()) / to_inexact(datum.denominator().as<exact_integer>());
  }

  /* ---- Arithmetic Operation Dispatcher ----------------------------------- */

  #define BOILERPLATE(SYMBOL)                                                  \
  let operator SYMBOL(const exact_integer& lhs, const object& rhs)             \
  {                                                                            \
    static const std::unordered_map<                                           \
      std::type_index,                                                         \
      std::function<object (const exact_integer&, const object&)>              \
    >                                                                          \
    overloads                                                                  \
    {                                                                          \
      {                                                                        \
        typeid(ratio), [](auto&& lhs, auto&& rhs)                              \
        {                                                                      \
          if (auto result { lhs SYMBOL rhs.template as<ratio>() }; result.reduce().is_integer()) \
          {                                                                    \
            return result.numerator();                                         \
          }                                                                    \
          else                                                                 \
          {                                                                    \
            return make(result);                                               \
          }                                                                    \
        }                                                                      \
      },                                                                       \
      {                                                                        \
        typeid(exact_integer), [](auto&& lhs, auto&& rhs)                      \
        {                                                                      \
          return make(lhs SYMBOL rhs.template as<exact_integer>());            \
        }                                                                      \
      },                                                                       \
      {                                                                        \
        typeid(single_float), [](auto&& lhs, auto&& rhs)                       \
        {                                                                      \
          return make(lhs SYMBOL rhs.template as<single_float>());             \
        }                                                                      \
      },                                                                       \
      {                                                                        \
        typeid(double_float), [](auto&& lhs, auto&& rhs)                       \
        {                                                                      \
          return make(lhs SYMBOL rhs.template as<double_float>());             \
        }                                                                      \
      },                                                                       \
    };                                                                         \
                                                                               \
    if (auto iter { overloads.find(rhs.type()) }; iter != std::end(overloads)) \
    {                                                                          \
      return std::invoke(cdr(*iter), lhs, rhs);                                \
    }                                                                          \
    else                                                                       \
    {                                                                          \
      throw error("no viable operation '" #SYMBOL " with ", lhs, " and ", rhs); \
    }                                                                          \
  } static_assert(true)

  BOILERPLATE(*);
  BOILERPLATE(+);
  BOILERPLATE(-);
  BOILERPLATE(/);
  BOILERPLATE(%);

  #undef BOILERPLATE

  /* ---- Arithmetic Comparison Dispatcher ---------------------------------- */

  #define BOILERPLATE(NUMBER, SYMBOL)                                          \
  auto operator SYMBOL(const NUMBER& lhs, const object& rhs) -> bool           \
  {                                                                            \
    if (rhs)                                                                   \
    {                                                                          \
      if (rhs.is<exact_integer>())                                             \
      {                                                                        \
        return lhs SYMBOL rhs.as<exact_integer>();                             \
      }                                                                        \
      else if (rhs.is<ratio>())                                                \
      {                                                                        \
        return lhs SYMBOL rhs.as<ratio>();                                     \
      }                                                                        \
      else if (rhs.is<single_float>())                                         \
      {                                                                        \
        return lhs SYMBOL rhs.as<single_float>();                              \
      }                                                                        \
      else if (rhs.is<double_float>())                                         \
      {                                                                        \
        return lhs SYMBOL rhs.as<double_float>();                              \
      }                                                                        \
    }                                                                          \
                                                                               \
    throw error("no viable operation '" #SYMBOL " with ", lhs, " and ", rhs);  \
  } static_assert(true)

  BOILERPLATE(exact_integer, !=);
  BOILERPLATE(exact_integer, < );
  BOILERPLATE(exact_integer, <=);
  BOILERPLATE(exact_integer, ==);
  BOILERPLATE(exact_integer, > );
  BOILERPLATE(exact_integer, >=);

  BOILERPLATE(ratio, !=);
  BOILERPLATE(ratio, < );
  BOILERPLATE(ratio, <=);
  BOILERPLATE(ratio, ==);
  BOILERPLATE(ratio, > );
  BOILERPLATE(ratio, >=);

  #undef BOILERPLATE
} // namespace kernel
} // namespace meevax
