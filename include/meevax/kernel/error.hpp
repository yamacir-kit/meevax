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

#ifndef INCLUDED_MEEVAX_KERNEL_ERROR_HPP
#define INCLUDED_MEEVAX_KERNEL_ERROR_HPP

#include <stdexcept>

#include <meevax/kernel/list.hpp>
#include <meevax/kernel/string.hpp>
#include <meevax/type_traits/underlying_cast.hpp>

/* ---- Error ------------------------------------------------------------------
 *
 * - error
 *    |-- file-error
 *    |-- read_error
 *    |    `-- unexpected_character
 *    `-- syntax_error
 *
 * -------------------------------------------------------------------------- */

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

    auto irritants() const noexcept -> const_reference;

    auto message() const noexcept -> const_reference;

    virtual auto raise() const -> void;

    virtual auto what() const -> std::string;
  };

  auto operator <<(std::ostream &, error const&) -> std::ostream &;

  #define DEFINE_ERROR(TYPENAME)                                               \
  struct TYPENAME ## _error : public error                                     \
  {                                                                            \
    using error::error;                                                        \
                                                                               \
    ~TYPENAME ## _error() override = default;                                  \
  }

  DEFINE_ERROR(file);
  DEFINE_ERROR(read);
  DEFINE_ERROR(syntax);

  template <char C>
  struct unexpected_character : public read_error
  {
    using read_error::read_error;

    ~unexpected_character() override = default;

    static auto get() -> auto const&
    {
      static auto const result = unexpected_character(make<string>("unexpected character"), make<character>(C));
      return result;
    }
  };

  template <typename... Ts>
  struct tagged_read_error : public read_error
  {
    using read_error::read_error;

    ~tagged_read_error() override = default;
  };

  template <typename... Ts>
  struct tagged_syntax_error : public syntax_error
  {
    using syntax_error::syntax_error;

    ~tagged_syntax_error() override = default;
  };

  template <typename Thunk>
  auto with_exception_handler(Thunk && thunk)
  {
    try
    {
      return thunk();
    }

    catch (exit_status const value) // NOTE: emergency-exit
    {
      return underlying_cast(value);
    }

    catch (pair::const_reference error) // NOTE: default-exception-handler (Terminate the program without running any outstanding dynamic-wind after procedures)
    {
      std::cerr << header("exception-handler") << error << std::endl;

      return underlying_cast(exit_status::failure);
    }

    catch (error const& error) // NOTE: system-error
    {
      std::cerr << header("system-error") << error.what() << std::endl;

      return underlying_cast(exit_status::failure);
    }

    catch (std::exception const& error)
    {
      std::cerr << header("kernel-error") << error.what() << std::endl;

      return underlying_cast(exit_status::failure);
    }

    catch (...)
    {
      std::cerr << header("unknown-error") << "An unknown object was thrown that was neither a Meevax exception type nor a C++ standard exception type." << std::endl;

      return underlying_cast(exit_status::failure);
    }
  }

  auto invalid_application(pair::const_reference) -> error;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_ERROR_HPP
