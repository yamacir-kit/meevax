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

#ifndef INCLUDED_MEEVAX_KERNEL_DEBUGGER_HPP
#define INCLUDED_MEEVAX_KERNEL_DEBUGGER_HPP

namespace meevax
{
inline namespace kernel
{
  template <typename Module>
  class debugger
  {
    friend Module;

    IMPORT(Module, standard_debug_port, const);

  public:

    struct tracker
    {
      explicit tracker(let const&)
      {
        std::cout << header("debugger") << "construct tracker " << this << std::endl;
      }

      ~tracker()
      {
        std::cout << header("debugger") << "destruct tracker " << this << std::endl;
      }

      friend auto operator <<(std::ostream & os, tracker const& datum) -> std::ostream &
      {
        return os << magenta << "#("
                  << green << "tracker" << reset
                  << faint << " #;" << &datum << reset
                  << magenta << ")" << reset;
      }
    };
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_DEBUGGER_HPP
