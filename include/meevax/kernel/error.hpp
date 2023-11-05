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

#ifndef INCLUDED_MEEVAX_KERNEL_ERROR_HPP
#define INCLUDED_MEEVAX_KERNEL_ERROR_HPP

#include <meevax/kernel/pair.hpp>
#include <meevax/kernel/string.hpp>

namespace meevax
{
inline namespace kernel
{
  struct error : public virtual pair // (<message> . <irritants>)
  {
    using pair::pair;

    auto irritants() const noexcept -> object const&;

    auto message() const noexcept -> object const&;

    [[noreturn]] // NOTE: GCC ignores this attribute when accessed through pointer (https://gcc.gnu.org/bugzilla/show_bug.cgi?id=84476)
    virtual auto raise() const -> void;

    auto what() const -> std::string;
  };

  auto operator <<(std::ostream &, error const&) -> std::ostream &;

  /*
     - error
        |-- file-error
        `-- read_error
  */
  #define DEFINE_ERROR(TYPENAME)                                               \
  struct TYPENAME ## _error : public error                                     \
  {                                                                            \
    using error::error;                                                        \
                                                                               \
    auto raise() const -> void override                                        \
    {                                                                          \
      throw *this;                                                             \
    }                                                                          \
  }

  DEFINE_ERROR(file);
  DEFINE_ERROR(read);

  template <typename Thunk>
  auto with_exception_handler(Thunk && thunk)
  {
    try
    {
      thunk();
      return EXIT_SUCCESS;
    }
    catch (int const status) // NOTE: emergency-exit
    {
      gc.clear();
      return status;
    }
    catch (error const& error)
    {
      std::cerr << error << std::endl;
      return EXIT_FAILURE;
    }
    catch (std::exception const& exception)
    {
      std::cerr << error(make<string>(exception.what())) << std::endl;
      return EXIT_FAILURE;
    }
  }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_ERROR_HPP
