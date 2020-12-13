#include <meevax/kernel/number.hpp>

namespace meevax
{
inline namespace kernel
{
  /* ---- Multi-Precision Exact-Integer ------------------------------------- */

  [[deprecated]]
  auto to_inexact(const exact_integer& datum) -> default_float
  {
    return default_float(datum.value.convert_to<default_float::value_type>());
  }

  auto operator + (exact_integer const& a, ratio const& b) -> ratio { return ratio(a * b.denominator() + b.numerator(), b.denominator()); }
  auto operator - (exact_integer const& a, ratio const& b) -> ratio { return ratio(a * b.denominator() - b.numerator(), b.denominator()); }
  auto operator * (exact_integer const& a, ratio const& b) -> ratio { return ratio(a * b.numerator(), b.denominator()); }
  auto operator / (exact_integer const& a, ratio const& b) -> ratio { return a * b.invert(); }

  auto operator % (exact_integer const&, ratio const& rhs) -> ratio
  {
    return rhs; // TODO
  }

  /* ---- Ratio ------------------------------------------------------------- */

  auto to_inexact(const ratio& datum) -> default_float
  {
    return to_inexact(datum.numerator().as<exact_integer>()) / to_inexact(datum.denominator().as<exact_integer>());
  }

  auto ratio::reduce() -> const ratio&
  {
    if (const exact_integer divisor {
          boost::multiprecision::gcd(
            numerator().as<exact_integer>().value,
            denominator().as<exact_integer>().value)
        };
        not divisor.is(1))
    {
      numerator() = make(numerator().as<exact_integer>() / divisor);
      denominator() = make(denominator().as<exact_integer>() / divisor);
    }

    return *this;
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

  /* ---- Arithmetic Comparisons -------------------------------------------- */

  #define BOILERPLATE(SYMBOL)                                                  \
  auto operator SYMBOL(const exact_integer& lhs, ratio& rhs) -> bool           \
  {                                                                            \
    if (rhs.reduce().is_integer())                                             \
    {                                                                          \
      return lhs SYMBOL rhs.numerator();                                       \
    }                                                                          \
    else                                                                       \
    {                                                                          \
      return false;                                                            \
    }                                                                          \
  } static_assert(true)

  BOILERPLATE(!=);
  BOILERPLATE(<);
  BOILERPLATE(<=);
  BOILERPLATE(==);
  BOILERPLATE(>);
  BOILERPLATE(>=);

  #undef BOILERPLATE

  #define BOILERPLATE(SYMBOL)                                                  \
  auto operator SYMBOL(const ratio& lhs, const exact_integer& rhs) -> bool     \
  {                                                                            \
    auto copy { lhs };                                                         \
                                                                               \
    if (copy.reduce().is_integer())                                            \
    {                                                                          \
      return copy.numerator().as<exact_integer>() SYMBOL rhs;                  \
    }                                                                          \
    else                                                                       \
    {                                                                          \
      return false;                                                            \
    }                                                                          \
  } static_assert(true)

  BOILERPLATE(!=);
  BOILERPLATE(<);
  BOILERPLATE(<=);
  BOILERPLATE(==);
  BOILERPLATE(>);
  BOILERPLATE(>=);

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
