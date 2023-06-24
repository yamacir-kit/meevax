/*
   Copyright 2018-2023 Tatsuya Yamasaki.

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

#ifndef INCLUDED_MEEVAX_KERNEL_CONTINUATION_HPP
#define INCLUDED_MEEVAX_KERNEL_CONTINUATION_HPP

#include <meevax/kernel/list.hpp>

namespace meevax
{
inline namespace kernel
{
  struct continuation : public virtual pair
  {
    using pair::pair;
  };

  auto operator <<(std::ostream &, continuation const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_CONTINUATION_HPP
