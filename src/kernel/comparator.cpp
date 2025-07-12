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

#include <meevax/kernel/box.hpp>
#include <meevax/kernel/list.hpp>

namespace meevax::inline kernel
{
  auto find(object & b) -> object &
  {
    if (let & x = car(b); x.is<box>())
    {
      return x = find(x);
    }
    else
    {
      return b;
    }
  }

  auto union_find(object const& x, object const& y, std::unordered_map<object, object> & forest)
  {
    if (auto iterator_x = forest.find(x); iterator_x != forest.end())
    {
      if (auto iterator_y = forest.find(y); iterator_y != forest.end())
      {
        auto&& root_x = find(iterator_x->second);
        auto&& root_y = find(iterator_y->second);

        if (eq(root_x, root_y))
        {
          return true;
        }
        else
        {
          if (auto rank_x = car(root_x).as<small_integer>(),
                   rank_y = car(root_y).as<small_integer>(); rank_x > rank_y)
          {
            car(root_x) = make<small_integer>(rank_x + rank_y);
            car(root_y) = root_x;
          }
          else
          {
            car(root_x) = root_y;
            car(root_y) = make<small_integer>(rank_x + rank_y);
          }
        }
      }
      else
      {
        forest.emplace(y, find(iterator_x->second));
      }
    }
    else
    {
      if (auto iterator_y = forest.find(y); iterator_y != forest.end())
      {
        forest.emplace(x, find(iterator_y->second));
      }
      else
      {
        let const b = make<box>(make<small_integer>(1));
        forest.emplace(x, b);
        forest.emplace(y, b);
      }
    }

    return false;
  }

  /*
     Efficient Nondestructive Equality Checking for Trees and Graphs
  */
  auto equal(object const& x, object const& y, std::unordered_map<object, object> & forest) -> bool
  {
    return eqv(x, y) or x.equal2(y) or (x.is<pair>() and
                                        y.is<pair>() and (union_find(x, y, forest) or (equal(car(x), car(y), forest) and
                                                                                       equal(cdr(x), cdr(y), forest))));
  }

  auto equal(object const& x, object const& y) -> bool
  {
    auto forest = std::unordered_map<object, object>();
    return equal(x, y, forest);
  }
} // namespace meevax::kernel
