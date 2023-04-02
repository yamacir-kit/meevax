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

#include <sstream>

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
    if (auto ss = std::stringstream(); not std::empty(allocation_counts))
    {
      for (auto&& [type, value] : allocation_counts)
      {
        ss << demangle(type.name()) << "\t" << value << "\n";
      }

      sh("echo \"" + ss.str() + "\" | sed 's/meevax::kernel:://g'              \
                                    | sort --field-separator='\t'              \
                                           --key=2                             \
                                           --numeric-sort                      \
                                           --reverse                           \
                                    | echo \"TYPENAME\tALLOCATION COUNT\n$(cat -)\" \
                                    | column -t -s'\t'");
    }

    if (auto ss = std::stringstream(); not std::empty(instruction_fetchs))
    {
      for (auto&& [instruction, count] : instruction_fetchs)
      {
        ss << instruction << "\t" << count << "\n";
      }

      sh("echo \"" + ss.str() + "\" | sed 's/meevax::kernel:://g'              \
                                    | sort --field-separator='\t'              \
                                           --key=2                             \
                                           --numeric-sort                      \
                                           --reverse                           \
                                    | echo \"MNEMONIC\tFETCH COUNT\n$(cat -)\" \
                                    | column -t -s'\t'");
    }
  }

  auto current_profiler() -> profiler &
  {
    static profiler value {};
    return value;
  }
} // namespace kernel
} // namespace meevax
