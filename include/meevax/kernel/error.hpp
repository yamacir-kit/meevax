/*
   Copyright 2018-2024 Tatsuya Yamasaki.

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

namespace meevax::inline kernel
{
  struct error : public virtual pair // (<message> . <irritants>)
               , public std::exception
  {
    mutable std::string cache {};

    using pair::pair;

    ~error() override = default;

    auto irritants() const noexcept -> object const&;

    virtual auto make() const -> object;

    auto message() const noexcept -> object const&;

    /*
       GCC ignores this attribute when accessed through pointer
       (https://gcc.gnu.org/bugzilla/show_bug.cgi?id=84476)
    */
    [[noreturn]]
    virtual auto raise() const -> void;

    auto report(std::ostream &) const -> std::ostream &;

    auto what() const noexcept -> char const* override;
  };

  auto operator <<(std::ostream &, error const&) -> std::ostream &;

  struct file_error : public error
  {
    using error::error;

    auto make() const -> object override
    {
      return meevax::make(*this);
    }

    auto raise() const -> void override
    {
      throw *this;
    }
  };

  struct read_error : public error
  {
    using error::error;

    auto make() const -> object override
    {
      return meevax::make(*this);
    }

    auto raise() const -> void override
    {
      throw *this;
    }
  };

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
      return status;
    }
    catch (error const& error)
    {
      error.report(std::cerr);
      return EXIT_FAILURE;
    }
    catch (std::exception const& exception)
    {
      error(make<string>(exception.what())).report(std::cerr);
      return EXIT_FAILURE;
    }
  }
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_ERROR_HPP
