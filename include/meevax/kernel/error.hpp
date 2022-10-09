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

#ifndef INCLUDED_MEEVAX_KERNEL_ERROR_HPP
#define INCLUDED_MEEVAX_KERNEL_ERROR_HPP

#include <meevax/kernel/pair.hpp>

namespace meevax
{
inline namespace kernel
{
  enum class exit_status : int
  {
    success = EXIT_SUCCESS,
    failure = EXIT_FAILURE,
  };

  struct error : public virtual pair
  {
    using pair::pair;

    explicit error(std::string const&, const_reference = unit);

    auto irritants() const noexcept -> const_reference;

    auto message() const noexcept -> const_reference;

    [[noreturn]] // NOTE: GCC ignores this attribute when accessed through pointer (https://gcc.gnu.org/bugzilla/show_bug.cgi?id=84476)
    virtual auto raise() const -> void;

    auto what() const -> std::string;
  };

  auto operator <<(std::ostream &, error const&) -> std::ostream &;

  /*
     - error
        |-- file-error
        |-- read_error
        `-- syntax_error
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
  DEFINE_ERROR(syntax);

  template <typename Thunk>
  auto with_exception_handler(Thunk && thunk)
  {
    try
    {
      return thunk();
    }

    catch (exit_status const value) // NOTE: emergency-exit
    {
      gc.clear(); // NOTE:
      return underlying_cast(value);
    }

    catch (const_reference error) // NOTE: procedure `throw` (Terminate the program without running any outstanding dynamic-wind after procedures)
    {
      std::cerr << "; " << error << std::endl;
      return underlying_cast(exit_status::failure);
    }

    catch (error const& error) // NOTE: system-error
    {
      std::cerr << "; " << error << std::endl;
      return underlying_cast(exit_status::failure);
    }

    catch (std::exception const& error)
    {
      std::cerr << "; system-error " << std::quoted(error.what()) << std::endl;
      return underlying_cast(exit_status::failure);
    }

    catch (...)
    {
      std::cerr << "; error: An unknown object was thrown that was neither a Meevax exception type nor a C++ standard exception type." << std::endl;
      return underlying_cast(exit_status::failure);
    }
  }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_ERROR_HPP
