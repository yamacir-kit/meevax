/*
   Copyright 2018-2025 Tatsuya Yamasaki.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

#ifndef INCLUDED_MEEVAX_IOSTREAM_LEXICAL_CAST_HPP
#define INCLUDED_MEEVAX_IOSTREAM_LEXICAL_CAST_HPP

#include <sstream>
#include <stdexcept>

#include <meevax/utility/demangle.hpp>

namespace meevax::inline iostream
{
  template <typename T = std::string, typename... Ts>
  auto lexical_cast(Ts&&... xs) -> T
  {
    if (std::stringstream ss; (ss << ... << xs))
    {
      if constexpr (std::is_same_v<std::decay_t<T>, std::string>)
      {
        return ss.str();
      }
      else
      {
        if (auto x = T(); ss >> x)
        {
          return x;
        }
        else
        {
          auto what = std::stringstream();
          what << "failed to deserialize " << demangle(typeid(T)) << " type object";
          throw std::runtime_error(what.str());
        }
      }
    }
    else
    {
      auto what = std::stringstream();
      ((what << "failed to serialize"), ..., (what << " " << demangle(typeid(Ts)))) << " type object";
      throw std::runtime_error(what.str());
    }
  }
} // namespace meevax::iostream

#endif // INCLUDED_MEEVAX_IOSTREAM_LEXICAL_CAST_HPP
