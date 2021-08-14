/*
   Copyright 2018-2021 Tatsuya Yamasaki.

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
#include <typeinfo>

namespace meevax
{
inline namespace iostream
{
  template <typename To, typename From>
  auto lexical_cast(From const& from) -> To
  {
    if (std::stringstream interpreter; interpreter << from)
    {
      if (To to; interpreter >> to)
      {
        return to;
      }
      else
      {
        std::stringstream what;
        what << "failed to read " << typeid(To).name() << " type object from std::stringstream";
        throw std::runtime_error(what.str());
      }
    }
    else
    {
      std::stringstream what;
      what << "failed to write " << typeid(From).name() << " type object to std::stringstream";
      throw std::runtime_error(what.str());
    }
  }
} // namespace iostream
} // namespace meevax

#endif // INCLUDED_MEEVAX_IOSTREAM_LEXICAL_CAST_HPP
