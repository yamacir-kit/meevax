#include <meevax/kernel/number.hpp>

namespace meevax
{
inline namespace kernel
{
  auto operator * (exact_integer const& a, exact_integer const& b) -> exact_integer { return static_cast<exact_integer>(a.value * b.value); }
  auto operator + (exact_integer const& a, exact_integer const& b) -> exact_integer { return static_cast<exact_integer>(a.value + b.value); }
  auto operator - (exact_integer const& a, exact_integer const& b) -> exact_integer { return static_cast<exact_integer>(a.value - b.value); }
  auto operator / (exact_integer const& a, exact_integer const& b) -> exact_integer { return static_cast<exact_integer>(a.value / b.value); }
  auto operator % (exact_integer const& a, exact_integer const& b) -> exact_integer { return static_cast<exact_integer>(a.value % b.value); } // TODO ratio
  auto operator !=(exact_integer const& a, exact_integer const& b) -> bool { return a.value != b.value; }
  auto operator < (exact_integer const& a, exact_integer const& b) -> bool { return a.value <  b.value; }
  auto operator <=(exact_integer const& a, exact_integer const& b) -> bool { return a.value <= b.value; }
  auto operator ==(exact_integer const& a, exact_integer const& b) -> bool { return a.value == b.value; }
  auto operator > (exact_integer const& a, exact_integer const& b) -> bool { return a.value >  b.value; }
  auto operator >=(exact_integer const& a, exact_integer const& b) -> bool { return a.value >= b.value; }

  auto operator + (exact_integer const& a, ratio const& b) -> ratio { return ratio(a * b.denominator() + b.numerator(), b.denominator()); }
  auto operator - (exact_integer const& a, ratio const& b) -> ratio { return ratio(a * b.denominator() - b.numerator(), b.denominator()); }
  auto operator * (exact_integer const& a, ratio const& b) -> ratio { return ratio(a * b.numerator(), b.denominator()); }
  auto operator / (exact_integer const& a, ratio const& b) -> ratio { return a * b.invert(); }
  auto operator % (exact_integer const&,   ratio const& b) -> ratio { return b; } // TODO
  auto operator !=(exact_integer const& a, ratio const& b) -> bool { return b.reduce().is_integer() ? a != b.numerator() : false; }
  auto operator < (exact_integer const& a, ratio const& b) -> bool { return b.reduce().is_integer() ? a <  b.numerator() : false; }
  auto operator <=(exact_integer const& a, ratio const& b) -> bool { return b.reduce().is_integer() ? a <= b.numerator() : false; }
  auto operator ==(exact_integer const& a, ratio const& b) -> bool { return b.reduce().is_integer() ? a == b.numerator() : false; }
  auto operator > (exact_integer const& a, ratio const& b) -> bool { return b.reduce().is_integer() ? a >  b.numerator() : false; }
  auto operator >=(exact_integer const& a, ratio const& b) -> bool { return b.reduce().is_integer() ? a >= b.numerator() : false; }

  #define BOILERPLATE(SYMBOL)                                                  \
  auto operator SYMBOL(ratio const& lhs, exact_integer const& rhs) -> bool     \
  {                                                                            \
    if (auto copy { lhs }; copy.reduce().is_integer())                         \
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

  auto operator + (ratio const& a, ratio const& b) -> ratio { return ratio(a.numerator() * b.denominator() + b.numerator() * a.denominator(), a.denominator() * b.denominator()); }
  auto operator - (ratio const& a, ratio const& b) -> ratio { return ratio(a.numerator() * b.denominator() - b.numerator() * a.denominator(), a.denominator() * b.denominator()); }
  auto operator * (ratio const& a, ratio const& b) -> ratio { return ratio(a.numerator() *                   b.numerator(),                   a.denominator() * b.denominator()); }
  auto operator / (ratio const& a, ratio const& b) -> ratio { return a * b.invert(); }
  auto operator % (ratio const&,   ratio const& b) -> ratio { return b; } // TODO
  auto operator ==(ratio const& a, ratio const& b) -> bool { return (a.numerator() * b.denominator()).binding() == (b.numerator() * a.denominator()); }
  auto operator !=(ratio const& a, ratio const& b) -> bool { return (a.numerator() * b.denominator()).binding() != (b.numerator() * a.denominator()); }
  auto operator < (ratio const& a, ratio const& b) -> bool { return (a.numerator() * b.denominator()).binding() <  (b.numerator() * a.denominator()); }
  auto operator <=(ratio const& a, ratio const& b) -> bool { return (a.numerator() * b.denominator()).binding() <= (b.numerator() * a.denominator()); }
  auto operator > (ratio const& a, ratio const& b) -> bool { return (a.numerator() * b.denominator()).binding() >  (b.numerator() * a.denominator()); }
  auto operator >=(ratio const& a, ratio const& b) -> bool { return (a.numerator() * b.denominator()).binding() >= (b.numerator() * a.denominator()); }

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
