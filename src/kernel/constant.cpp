#include <meevax/kernel/constant.hpp>

namespace meevax
{
inline namespace kernel
{
  std::unordered_map<std::string, let> const constants
  {
    // R7RS 7.1.1. Lexical structure
    { "+inf.0", make<system_float>(+system_float::infinity()) },
    { "-inf.0", make<system_float>(-system_float::infinity()) },

    { "+nan.0", make<system_float>(+system_float::quiet_NaN()) },
    { "-nan.0", make<system_float>(-system_float::quiet_NaN()) },

    // SRFI-144
    { "fl-pi", make<system_float>(boost::math::constants::pi<system_float::value_type>()) },
  };
} // namespace kernel
} // namespace meevax
