/*
   Copyright 2018-2022 Tatsuya Yamasaki.

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

#include <fstream>

#include <meevax/kernel/profiler.hpp>
#include <meevax/utility/demangle.hpp>

namespace meevax
{
inline namespace kernel
{
  auto sh(std::string const& command)
  {
    if (auto status = std::system(command.c_str()); status < 0)
    {
      std::exit(EXIT_FAILURE);
    }
    else
    {
      return WIFEXITED(status);
    }
  }

  profiler::~profiler()
  {
    if (auto file = std::ofstream("/tmp/meevax-profile-by-type.txt"); file)
    {
      for (auto&& [type, topic] : by_type)
      {
        file << topic.allocation << " " << demangle(type.name()) << "\n";
      }
    }

    sh("cat /tmp/meevax-profile-by-type.txt | sed 's/meevax::kernel:://g' | sort -rn | column -t");
  }

  auto current_profiler() -> profiler &
  {
    static profiler value {};
    return value;
  }
} // namespace kernel
} // namespace meevax
